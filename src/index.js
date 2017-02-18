var config = require("./db");
var layout = require('aframe-layout').component;

AFRAME.registerComponent('layout', layout);

// https://firebase.google.com/docs/reference/js/firebase.database.OnDisconnect

firebase.initializeApp(config);

firebase.auth().signInAnonymously().catch(function(error) {
    console.error("Login error: "+error.message);
});

firebase.auth().onAuthStateChanged(function(user) {
  if (user) {
    var isAnonymous = user.isAnonymous;
    var uid = user.uid;
    console.log("Logged in");
  } else {
    console.log("Not logged in");
  }
});

var db = firebase.database();

var votes = db.ref('votes');
votes.on('value', function(snapshot) {
    setVotes(snapshot.val());
});

var Elm = require( './Main' );
var app = Elm.Main.embed(document.body);

var node = null;

app.ports.setUser.subscribe(function(user) {
    node = db.ref('votes/'+user);
    node.set(-1);
    node.onDisconnect().remove();
});

app.ports.setVote.subscribe(function(vote) {
    node.set(vote);
});

function setVotes(val) {
    console.log(val);
    var votes = [];
    for (var key in val) {
        votes.push(
            {
                user:key,
                vote:val[key]
            }
        )
    }
    console.log(votes);
    app.ports.updateVotes.send(votes);
}
