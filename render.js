"use strict"

import 'whatwg-fetch';
import "regenerator-runtime/runtime";


var wave = window.wave = null;
var waves = window.waves = {}

// initialize a var for the port Sub, define in `register`
var sendLoad, sendFinish;

const nodes =
  { miniMaker: "#minimaker"
  , songDesigner: "#song-designer"
  , quickEditor: "#chart-designer"
  }


init();

// Checks the Ghost endpoint to see if a user is logged in
async function isMember() {
  let resp = fetch("/members/api/member");

  return resp.then(r =>
     Promise.resolve( 200 == r.status ) );
}

async function getMember() {
  if (! await isMember() ) return false;

  let resp = fetch("/members/api/member");

  return resp.then(r => r.json());
}

// Transforms the document.cookie string to a JavaScript object
function getCookies () {
  return document.cookie.split(";").reduce((all, str) => {
    let k = str.slice(0, str.indexOf("="));
    let v = str.slice(1+str.indexOf("="));
     all[k] = v;
     return all;
  }, {})
}


// Puts the expiriation date of login in localStorage
function setLoginExpiry() {
  let expiry = new Date();
  expiry.setDate(expiry.getDate() + 1);
  localStorage.loginExpiry = expiry;
}

// Puts the member data in localstorage
function storeMember( data ) {
  localStorage.member = JSON.stringify(data);
}

function getStoredMember() {
  return JSON.parse(localStorage.member);
}


function cleanupStorage() {
  delete localStorage.loginExpiry;
  delete localStorage.member;
}

// Checks to see if the current login has expired
function isLoginExpired() {
  let prev = new Date( localStorage.loginExpiry );
  if ( !prev ) return true;

  let now = new Date();
  return now > prev;
}


function isLoginValid() {
  if (!isLoginExpired()) return true;

  let prev = localStorage.loginExpiry;
  let now = new Date();

  return now > prev;
}


// Determines if the member is logged in
async function isActiveMember() {
  if ( isLoginExpired() ) {
    cleanupStorage();
    return false;
  }

  let current = await getMember();
  if ( current ) {
    setLoginExpiry();
    storeMember( current );
    return true;
  }

  return false
}


function loadWaveSurfer(cb) {
  if (typeof WaveSurfer !== "function") {
    const node = document.querySelector("head");
    const s = document.createElement("script");
    s.src = "/assets/song-designer/wavesurfer.js";
    s.onload = cb;
    node.appendChild(s);
  } else {
    cb();
  }
}


/** Depends on the global wave object to be ready to play. */
function playMusic(nodeID) {
    getWave(nodeID).play();
}


function stopMusic(nodeID) {
    getWave(nodeID).stop();
}


/** Depends on the global wave object to be ready to pause. */
function pauseMusic(nodeID) {
    getWave(nodeID).pause();
}


function getWave(nodeID, source) {
  if (waves[nodeID]) return waves[nodeID];

  waves[nodeID] = WaveSurfer.create(waveOpts(nodeID));
  waves[nodeID].load(source);
  return waves[nodeID];
}


function setSource([nodeID, source]) {
  if (wave) {
    wave.empty();
    wave.load(source);
    wave.on('ready', e => {
      sendLoad([nodeID, source])
    });

    wave.on('finish', e => {
      sendFinish([nodeID, source])
    });
  } else {
    withElement(nodeID, () => createSource([nodeID, source]));
  }
}


function killWaveSurfer() {
  wave.destroy();
}


function waveOpts(nodeID) {
  if (nodeID.indexOf("#") == 0) { nodeID } 
  else { nodeID = "#" + nodeID }

  return {
    container: nodeID,
    waveColor: "violet",
    progressColor: "purple",
    responsive: true
  }
}


function createSource([nodeID, source]) {
  withElement(nodeID,  function makingWaveSurfer()  {
    const wave = getWave(nodeID, source);

    wave.load(source);
    wave.on("ready", e => {
      sendLoad([nodeID, source]);
    });

    wave.on('finish', e => {
      sendFinish([nodeID, source])
    });
  });
 }


const ports =
  { scrollTo : smoothScroll
  , setStorage : setStorage
  }


function getStored(name) {
  let val = localStorage.getItem(name);
  if (val) return JSON.parse(val);
}


/** Put an item in local storage from Elm. */
function setStorage([name, value]) {
  let prev;
  if (prev = getStored(name)) {
    if (Array.isArray(prev)) {
      value = [...new Set(prev.concat(value))];
    }
  }

  localStorage.setItem(name, JSON.stringify(value));
}


function deleteStorage(name) {
  delete localStorage[name];
};


async function checkAnonSongs(member) {
  let ids = getStored("trackIDs");
  if (ids && ids.length > 0) {
    let {uuid, email} = member;
    if (uuid && email) {
      let result = await assignAnonTracks(uuid, email, ids);
    }
  }
}


function removeTag(nodeId) {
  if (nodeId.indexOf("#") == 0) {
    return nodeId.slice(1);
  } 
  return nodeId;
}


/** Apply JavaScript callbacks to Elm callers. */
function register(app) {
  // Wavesurfer callbacks

  app.ports.playMusic.subscribe(playMusic);

  app.ports.stopMusic.subscribe(stopMusic);

  app.ports.pauseMusic.subscribe(pauseMusic);

  app.ports.setSource.subscribe(setSource);

  app.ports.createSource.subscribe(createSource);

  app.ports.kill.subscribe(killWaveSurfer);

  sendLoad = ([nodeId, audioSrc]) => {
    app.ports.loadedTrack.send([removeTag(nodeId), audioSrc]);
  };
  
  sendFinish = ([nodeId, audioSrc]) => {
    app.ports.finishedTrack.send([removeTag(nodeId), audioSrc]);
  };
  
  // other

  for (let k in ports) {
    app.ports[k] && app.ports[k].subscribe(ports[k]);
  }
}


function initializeSongDesigner(member) {
  const root = document.querySelector (nodes.songDesigner);
  var app = Elm.Main.init( { flags: member, node: root } );

  checkAnonSongs(member);

  if (typeof Elm == "undefined" ) {
    throw new Error("The elm song designer was not found.");
  }

  loadWaveSurfer(() => register(app));
}


async function initMiniMaker(maybeMember) {
  var maybeMember = (await isActiveMember()) && getStoredMember();
  const root = document.querySelector(nodes.miniMaker);
  if (maybeMember) checkAnonSongs(maybeMember);
  var app = Elm.MiniMaker.init( {flags: maybeMember || null, node: root});

  loadWaveSurfer(() => register(app));
}


async function initQuickEditor(member) {
  // var maybeMember = (await isActiveMember()) && getStoredMember();
  const root = document.querySelector(nodes.quickEditor);
  var app = Elm.Designer.Chart.init( {flags: member, node: root} );


  loadWaveSurfer(() => register(app));
}


async function init() {
   console.log("Loading the new js");
  if (typeof Elm == "undefined") {
    console.error("Unable to initialize application. Load the elm scripts.");
    return;
  }

  if (document.querySelector(nodes.quickEditor)) {
    if (await isActiveMember()) {
      initQuickEditor(getStoredMember());
    } else {
      initQuickEditor(null);
    }
  }

  if (document.querySelector(nodes.miniMaker)) {
      // user does not need to be logged in
      initMiniMaker();
  }

  if (document.querySelector(nodes.songDesigner)) {
    if (await isActiveMember()) {
      initializeSongDesigner(getStoredMember());
    } else {
      initializeSongDesigner(null);
    }
  }
}


function withElement(selector, cb, count = 3, dt = 500) {
  if (document.querySelector(selector))
   return cb()
  else if (count > 0)
   return setTimeout(() => withElement(selector, cb, count-1), dt);
  else
   return console.warn("Did not find an element for your callback. Was seeking this selector: " + selector);
}


function smoothScroll(selector) {
  withElement(selector, function scrolling() {
    // jquery has a better animation than DOM
    jQuery("html").animate({scrollTop: $(selector).offset().top - 100}, 300);
  });

  // withElement(selector, function scrolling()  {
  //   var el = document.querySelector(selector);
  //   el.scrollIntoView({
  //       behavior: 'smooth'
  //   })}
  // );
}


async function assignAnonTracks(uuid, email, trackIDs = []) {
  return fetch("https://synthony.app/user",
    { method : "POST"
    , headers : { "Content-Type" : "application/json" }
    , body : JSON.stringify(
       { uuid : uuid
       , email : email
       , trackIDs : trackIDs
       , action : "assignAnonTracks"
       })
    })
    .then(r => r.json())
    .then(response => {
      if (response.success) {
        deleteStorage("trackIDs");
      }
    })
}


async function testInsertUser() {
  fetch("https://synthony.app/members/api/send-magic-link/",
    { method : "POST"
    , headers : { "Content-Type" : "application/json" }
    , body : JSON.stringify(
       { email: "ckburner2@gmail.com"
       , name : "Insert ckburner"
       , requestSrc: "minimaker"
       })
    })
    .then(r => r.text())
    .then(res => {
        console.log("has response:" ,res)
       if (res.indexOf("Created") == 0) {
         console.log("Successfully created the user")
       } else {
         console.log("The user did not get created.")
       }})
}


async function testAddUserTracks(trackIDs = [182,181,180], devmode = true) {
  let uuid  = (devmode)
    ? "263bb49a-a3e7-471e-a945-74570b25fd53"
    : "f6bc137f-218b-42c7-8a6f-ae445103d96c"

  let email = (devmode)
    ? "demo@mail.com"
    : "anon@synthony.app"

  fetch("http://localhost:3000/user",
    { method : "POST"
    , headers : { "Content-Type" : "application/json" }
    , body : JSON.stringify(
       { uuid : uuid
       , email : email
       , trackIDs : trackIDs
       , action : "assignAnonTracks"
       })
    })
    .then(r => r.json())
    .then(res => {
        console.log("has response:" ,res)

     })
}


async function testInsertUserBadEmail() {
  fetch("https://synthony.app/members/api/send-magic-link/",
    { method : "POST"
    , headers : { "Content-Type" : "application/json" }
    , body : JSON.stringify(
       { email: "ckburner2@no"
       , name : "Insert ckburner"
       , requestSrc: "minimaker"
       })
    })
    .then(r => r.text())
    .then(res => {
        console.log("has response:" ,res)
       if (resText == "Created.") {
         console.log("Successfully created the user")
       } else {
         console.log("The user did not get created.")
       }})
}
