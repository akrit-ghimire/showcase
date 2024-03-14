const canvasControl = {
    canvas: document.querySelector('canvas'),
    
    ctx: null,
    currentImage: null,
    nextImage: null,
    loop: null,

    timestamp: () => new Date().getTime(),
    timeStampElem: document.querySelector('#date'),

    imageURL: () => 'output.png?'+ canvasControl.timestamp(), // changing param means no cache image returning
    
    drawCurrentImage: () => { // draw current image on canvas if there is one
        canvasControl.ctx.clearRect(0, 0, canvasControl.canvas.width, canvasControl.canvas.height);
        if (canvasControl.currentImage) canvasControl.ctx.drawImage(canvasControl.currentImage, 0, 0, canvasControl.canvas.width, canvasControl.canvas.height);
    },

    preload: (url, callbackFunc) => { // make sure image is loaded before drawing it on screen (prevents canvas flickering)
        const img = new Image()
        img.onload = () => {callbackFunc(img)}
        img.src = url
    },
    
    updateCanvas: () => {
        canvasControl.preload(canvasControl.imageURL(), (img) => { // draw previous img first while waiting for next one to load -- prevents canvas flickering
            canvasControl.nextImg = img 
            if (canvasControl.currentImage) canvasControl.drawCurrentImage()
            canvasControl.currentImage = canvasControl.nextImg
            canvasControl.timeStampElem.innerText = "🐢... " + new Date().toLocaleString()
        })
    },

    startWatching: () => {
        console.log('watch started')
        canvasControl.loop = setInterval(canvasControl.updateCanvas, 500);
    },
    stopWatching: () => {
        console.log('watch ended')
        if (canvasControl.loop) clearInterval(canvasControl.loop)
    },

    __init__: () => {
        canvasControl.ctx = canvasControl.canvas.getContext('2d')
    }
}
canvasControl.__init__()


