#ifndef DOJOCLIENT_H
#define DOJOCLIENT_H

#include <QObject>
#include <QHash>
#include <QVector3D>
#include <QTimer>
#include <QTcpSocket>
#include <QUdpSocket>

#define TCP_PORT 5555

#define UDP_CLIENT_PORT 49389
#define UDP_SERVER_PORT 45907

typedef int dojoId;

struct dojoInput{
    float* data;
    float current_data;
};

class dojoClient : public QObject
{
    Q_OBJECT
public:
    explicit dojoClient( QString host, QObject *parent = 0);
    ~dojoClient();

    void connectToServer();
    void registerInput(dojoId id, float* data);
    void registerOutput(dojoId id, float* data);

signals:

public slots:
    void slotTcpReadyRead();
    void slotTcpDisconnected();
    void slotTimeout();
    void slotTcpError(QAbstractSocket::SocketError error);
    void slotUdpReadyRead();
    void slotUdpError(QAbstractSocket::SocketError error);

private:
    QString dojoHost;
    QHash<int, dojoInput*> inputs;
    QHash<int, float*> outputs;

    QTimer* sendTimer;
    QTcpSocket* tcpSocket;
    QUdpSocket* udpSocket;

    bool isTcpConnected;
};

#endif // DOJOCLIENT_H
