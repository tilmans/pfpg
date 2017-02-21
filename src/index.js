var layout = require('aframe-layout').component;

AFRAME.registerComponent('layout', layout);

var authenticated = false;
var myID = null;
var node = null;
var guser = null;
var firstCall = true;
function authenticate(user) {
    firebase.initializeApp(config);
    var db = firebase.database();

    firebase.auth().signInAnonymously().catch(function(error) {
        console.error("Login error: "+error.message);
    });

    firebase.auth().onAuthStateChanged(function(userdata) {
        console.log("Authenticated "+userdata.uid);
        if (firstCall) {
            firstCall = false;
            return;
        }

      if (userdata) {
        var isAnonymous = userdata.isAnonymous;
        myID = userdata.uid;
        authenticated = true;
        console.log("Logged in with "+myID);

        node = db.ref('votes/'+myID);
        node.set({user:user,vote:-1});
        // Seems like the session is kept
        // so can't delete without recreating 
        // later
        node.onDisconnect().remove();

        var votes = db.ref('votes');
        votes.on('value', function(snapshot) {
            setVotes(snapshot.val());
        });

      } else {
        console.log("Not logged in");
        authenticated = false;
      }
    });    
}

var Elm = require( './Main' );
var app = Elm.Main.embed(document.body);

app.ports.setUser.subscribe(function(user) {
    console.log("Authenticate "+user);
    guser = user;
    authenticate(user);
});

app.ports.setVote.subscribe(function(vote) {
    if (authenticated) {
        node.set({user:guser,vote:vote});
    }
});

function setVotes(val) {
    console.log(val);
    var votes = [];
    for (var key in val) {
        if (key != myID) {
            votes.push(
                {
                    id:key,
                    user:val[key].user,
                    vote:val[key].vote
                }
            )
        }
    }
    console.log(votes);
    app.ports.updateVotes.send(votes);
}
