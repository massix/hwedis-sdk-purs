import WebSocket from 'ws';
const messages = [];

const wait = () => {
  return new Promise(resolve => {
    setTimeout(() => resolve(), 1000);
  });
};

export const mkWebSocketImpl = address => id => () => {

  return new Promise((resolve, reject) => {
    const ws = new WebSocket(address, undefined, { headers: { 'user-agent': id } });
    ws.once('open', () => {
      resolve(ws);
      ws.pause();
    });
    ws.once('error', () => {
      reject('error');
    });
  });
};

export const sendImpl = ws => msg => async () => {
  ws.send(msg, { binary: false });
};

export const closeImpl = ws => code => reason => () => {
  return new Promise(resolve => {
    ws.close(code, reason);
    ws.once('close', code => {
      resolve(code);
      ws.pause();
      ws.terminate();
    });

    ws.resume();
  });
};

export const recvImpl = ws => () => {
  return new Promise(resolve => {
    ws.once('message', msg => {
      resolve('' + msg);
      ws.pause();
    });
    ws.resume();
  });
}
