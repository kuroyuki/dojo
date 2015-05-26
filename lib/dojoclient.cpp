#include "dojoclient.h"
#include <QTextStream>

dojoClient::dojoClient( QString host, QObject *parent) : QObject(parent)
{
    dojoHost = host;

    //create TCP socket
    tcpSocket = new QTcpSocket();
    connect(tcpSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(slotTcpError(QAbstractSocket::SocketError)));
    connect(tcpSocket, SIGNAL(readyRead()), this, SLOT(slotTcpReadyRead()));
    connect(tcpSocket, SIGNAL(disconnected()), this, SLOT(slotTcpDisconnected()));

    isTcpConnected = false;

    sendTimer = new QTimer(this);
    connect(sendTimer, SIGNAL(timeout()), this, SLOT(slotTimeout()));

    //connect UDP
    udpSocket = new QUdpSocket();
    connect(udpSocket, SIGNAL(readyRead()), this, SLOT(slotUdpReadyRead()));
    connect(udpSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(slotUdpError(QAbstractSocket::SocketError)));

    QTextStream(stdout)<<"dojoClient created"<<endl;
}

dojoClient::~dojoClient()
{

}
void dojoClient::connectToServer(){
    QTextStream(stdout)<<"connecting to dojo server"<<endl;
    sendTimer->start(10);
}
void dojoClient::registerInput(dojoId id, float* data){

    dojoInput* inputData = new dojoInput();
    inputData->current_data = 0;
    inputData->data = data;

    if (!inputs.contains(id)){
        if(isTcpConnected){
            QByteArray ba;
            int t = 0;
            ba.append(t);
            ba.append(id>>24);
            ba.append(id>>16);
            ba.append(id>>8);
            ba.append(id);

            tcpSocket->write(ba, ba.length());
            tcpSocket->waitForBytesWritten();
        }

        //save data with id
        inputs.insert(id, inputData);
    }
}
void dojoClient::registerOutput(dojoId id, float* data){
    if(!outputs.contains(id)){
        if(isTcpConnected){
            //send request to get node id
            QByteArray ba;
            ba.append(id>>24);
            ba.append(id>>16);
            ba.append(id>>8);
            ba.append(id);
            ba.prepend(0x01);

            tcpSocket->write(ba, ba.length());
            tcpSocket->waitForBytesWritten();
        }

        outputs.insert(id,  data);
    }
}
void dojoClient::slotTimeout(){
    //if TCP connected
    if(isTcpConnected){
        //send update via UDP
        QByteArray ba;
        QList<dojoId> list = inputs.keys();
        for(int i=0;i<list.size();i++){
            dojoId id = list[i];
            inputs.value(id)->current_data += *inputs.value(id)->data;
            //over threshlod -> generate ap
            if(inputs.value(id)->current_data >= 1){
                ba.append(id>>24);
                ba.append(id>>16);
                ba.append(id>>8);
                ba.append(id);

                inputs.value(id)->current_data = 0;
            }
        }
        if(ba.length()){
            udpSocket->writeDatagram(ba, QHostAddress(dojoHost), UDP_SERVER_PORT);
        }
    }
    else {
        //make connection with the server
        tcpSocket->connectToHost(dojoHost,TCP_PORT);
        if(tcpSocket->waitForConnected(1000)){

            //register all sensors
            QByteArray ba;
            QList<dojoId> list = inputs.keys();
            for(int i=0;i<list.size();i++){
                dojoId id = list[i];

                //send request to get node id
                int t = 0;
                ba.append(t);
                ba.append(id>>24);
                ba.append(id>>16);
                ba.append(id>>8);
                ba.append(id);

            }
            //register all actuators
            list = outputs.keys();
            for(int i=0;i<list.size();i++){
                dojoId id = list[i];

                //send request to get node id
                ba.append(0x01);
                ba.append(id>>24);
                ba.append(id>>16);
                ba.append(id>>8);
                ba.append(id);

            }

            tcpSocket->write(ba, ba.length());
            tcpSocket->waitForBytesWritten();
            isTcpConnected = true;

            QTextStream(stdout)<<"Tcp connected "<<endl;

            udpSocket->bind(QHostAddress::LocalHost, UDP_CLIENT_PORT);
        }
    }
}
void dojoClient::slotTcpReadyRead(){

}
void dojoClient::slotTcpDisconnected(){
    //wait for 1 sec and retry to connect
    QTextStream(stdout)<<"Tcp disconnected, reconnecting.. "<<endl;
    isTcpConnected = false;
}

void dojoClient::slotTcpError(QAbstractSocket::SocketError error){
    //wait for 1 sec and retry to connect
    QTextStream(stdout)<<"Tcp error, reconnecting.. "<<error<<endl;
    isTcpConnected = false;

    tcpSocket->close();
    udpSocket->close();
}
void dojoClient::slotUdpError(QAbstractSocket::SocketError error){
    //wait for 1 sec and retry to connect
    QTextStream(stdout)<<"Udp error, reconnecting.. "<<error<<endl;
    isTcpConnected = false;

    tcpSocket->close();
    udpSocket->close();
}

void dojoClient::slotUdpReadyRead(){
    while (udpSocket->hasPendingDatagrams()) {
        QByteArray datagram;
        datagram.resize(udpSocket->pendingDatagramSize());
        QHostAddress sender;
        quint16 senderPort;

        udpSocket->readDatagram(datagram.data(), datagram.size(),
                                &sender, &senderPort);

        while(datagram.size()>=4){
            dojoId id = 0;
            id  = (datagram[0]<<24) +  (datagram[1]<<16) +  (datagram[2]<<8) + datagram[3];
            float* data = outputs.value(id);
            *data = 1;

            datagram.remove(0,4);
        }
    }
}
