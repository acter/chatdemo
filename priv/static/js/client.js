var ws = new Object;  
var username;
var users;
var rid;
var base = 1000;
var increase = 25;
var reg = /^[a-zA-Z0-9_\u4e00-\u9fa5]+$/;
var LOGIN_ERROR = "There is no server to log in, please wait.";
var LENGTH_ERROR = "Name/Channel is too long or too short. 20 character max.";
var NAME_ERROR = "Bad character in Name/Channel. Can only have letters, numbers, Chinese characters, and '_'";
var DUPLICATE_ERROR = "Please change your name to login.";

util = {
	urlRE: /https?:\/\/([-\w\.]+)+(:\d+)?(\/([^\s]*(\?\S+)?)?)?/g,
	//  html sanitizer
	toStaticHTML: function(inputHtml) {
		inputHtml = inputHtml.toString();
		return inputHtml.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
	},
	//pads n with zeros on the left,
	//digits is minimum length of output
	//zeroPad(3, 5); returns "005"
	//zeroPad(2, 500); returns "500"
	zeroPad: function(digits, n) {
		n = n.toString();
		while(n.length < digits)
		n = '0' + n;
		return n;
	},
	//it is almost 8 o'clock PM here
	//timeString(new Date); returns "19:49"
	timeString: function(date) {
		var minutes = date.getMinutes().toString();
		var hours = date.getHours().toString();
		return this.zeroPad(2, hours) + ":" + this.zeroPad(2, minutes);
	},

	//does the argument only contain whitespace?
	isBlank: function(text) {
		var blank = /^\s*$/;
		return(text.match(blank) !== null);
	}
};
//always view the most recent message when it is added
function scrollDown(base) {
	window.scrollTo(0, base);
	$("#entry").focus();
};
// add message on board
function addMessage(from, target, text, time) {
	var name = (target == '*' ? 'all' : target);
	if(text === null) return;
	if(time == null) {
		// if the time is null or undefined, use the current time.
		time = new Date();
	} else if((time instanceof Date) === false) {
		// if it's a timestamp, interpret it
		time = new Date(time);
	}
	//every message you see is actually a table with 3 cols:
	//  the time,
	//  the person who caused the event,
	//  and the content
	var messageElement = $(document.createElement("table"));
	messageElement.addClass("message");
	// sanitize
	text = util.toStaticHTML(text);
	var content = '<tr>' + '  <td class="date">' + util.timeString(time) + '</td>' + '  <td class="nick">' + util.toStaticHTML(from) + ' says to ' + name + ': ' + '</td>' + '  <td class="msg-text">' + text + '</td>' + '</tr>';
	messageElement.html(content);
	//the log is the stream that we view
	$("#chatHistory").append(messageElement);
	base += increase;
	scrollDown(base);
};

function addSysMsg(text,time){
	if(text === null) return;
	if(time == null) {
		// if the time is null or undefined, use the current time.
		time = new Date();
	} else if((time instanceof Date) === false) {
		// if it's a timestamp, interpret it
		time = new Date(time);
	}
	var messageElement = $(document.createElement("table"));
	messageElement.addClass("message");
	text = util.toStaticHTML(text);
	var content = '<tr>' + '  <td class="date">' + util.timeString(time) + '</td>' + '  <td class="nick">'  + ' 系统消息 ' + ': ' + '</td>' + '  <td class="msg-text">' + text + '</td>' + '</tr>';
	messageElement.html(content);
	//the log is the stream that we view
	$("#chatHistory").append(messageElement);
	base += increase;
	scrollDown(base);
}
// set your name
function setName() {
	$("#name").text(username);
};

// set your room
function setRoom() {
	$("#room").text(rid);
};
// show error
function showError(content) {
	$("#loginError").text(content);
	$("#loginError").show();
};
// show login panel
function showLogin() {
	$("#loginView").show();
	$("#chatHistory").hide();
	$("#toolbar").hide();
	$("#loginError").hide();
	$("#loginUser").focus();
};
// show chat panel
function showChat() {
	$("#loginView").hide();
	$("#loginError").hide();
	$("#toolbar").show();
	$("entry").focus();
	scrollDown(base);
};

// add user in user list
function addUser(user) {
	var slElement = $(document.createElement("option"));
	slElement.attr("value", user);
	slElement.text(user);
	$("#usersList").append(slElement);
};

function send_msg(data){
	ws.send(JSON.stringify(data));  	
};
function login(username,rid){
	var data = {"msgid":1001,data:{"username":username,"rid":rid}};
	send_msg(data);
};
function getOnline(){
	var data = {"msgid":1005,data:{}};
	send_msg(data);
}

$(document).ready(function() {
	showLogin();

	//deal with login button click.
	$("#login").click(function() {
		username = $("#loginUser").attr("value");
		rid = $('#channelList').val();

		if(username.length > 20 || username.length == 0 || rid.length > 20 || rid.length == 0) {
			showError(LENGTH_ERROR);
			return false;
		}

		if(!reg.test(username) || !reg.test(rid)) {
			showError(NAME_ERROR);
			return false;
		}

		//query entry of connection
		if (!("WebSocket" in window)) {  
            alert("WebSocket NOT supported by your Browser!");  
            return;  
        }  
        ws = new WebSocket("ws://"+window.location.host+"/websocket");  
        ws.onopen = function() {  
            console.log('connected');  
   			setName();
			setRoom();
			showChat();
			$("#chatHistory").show();
			addSysMsg("Welcome to cowboy!");
			login(username,rid);
        };  

        ws.onmessage = function (evt) {  
        	console.log("Received: " + evt.data); 
            var data =  JSON.parse(evt.data);  
            console.log("Received: " + data); 
            if(data.msgid==1004){
            	var user = data.data;
            	console.log("username: " + user.username);
            	console.log("msg: " + user.msg);
            	addMessage(user.username, user.target, user.msg);
            }else if(data.msgid==1002){
            	addSysMsg("登录成功！");
            	//获取好友列表
            	// getOnline();
            	var users = data.data;
            	for(var i = 0; i < users.length; i++) {
					console.log("username: " + users[i]);
					var slElement = $(document.createElement("option"));
					slElement.attr("value", users[i]);
					slElement.text(users[i]);
					$("#usersList").append(slElement);
				}
            }else if (data.msgid==1006){
            	var user = data.data;
            	addUser(user)
            }             
        };  

        ws.onclose = function() {  
            console.log('close');  
        };  
	});

	//deal with chat mode.
	$("#entry").keypress(function(e) {
		var target = $("#usersList").val();
		if(e.keyCode != 13 /* Return */ ) return;
		var msg = $("#entry").attr("value").replace("\n", "");
		if(!util.isBlank(msg)) {
			// console.log(target);
			var data = {"msgid":1003,"data":{"username":username,"target":target,"msg":msg}};
			// console.log(JSON.stringify(data) );
			send_msg(data);
            $("#entry").attr("value", ""); // clear the entry field.
            addMessage(username, target, msg);
		}
	});
});