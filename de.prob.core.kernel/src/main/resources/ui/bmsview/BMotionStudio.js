bms = (function() {

	var extern = {}
	var session = Session();
	var svgCanvas = null;
	var isInit = false;
	var templateContent = null;
	var templateFile = null;
	
	$(document).ready(function() {
		
		hookInputFieldListener(true)

//		$('#bmsTab a:first').tab('show');

//		$('#bmsTab a').click(function(e) {
//			e.preventDefault();
//			$(this).tab('show');
//		})

//		$('#bmsTab a').on('shown', function(e) {
//			if ($(e.target).attr("href") == "#visualization") {
//				$("#result").html("")
//				forceRendering()
//				disableContextMenu()
//			} else {
//				$("#render").html("")
//				renderEdit()
//				initContextMenu()
//			}
			// e.relatedTarget // previous tab
//		})
		
	});
	
	$('#chooseTemplateBox').click(function(){
		
	    $("#sourceModal").on('shown', function() {
//        	editorHtml.refresh()
//        	editorJavascript.refresh()
        }).on('hidden', function() {
//        	renderEdit()
        });
        //Show Modal
        $("#sourceModal").modal('show');
        		
	})
	

	// --------------------------------------------
	// Helper functions
	// --------------------------------------------
	jQuery.fn.toHtmlString = function() {
		return $('<div></div>').html($(this).clone()).html();
	};
	
	jQuery.expr[':'].parents = function(a,i,m){
	    return jQuery(a).parents(m[3]).length < 1;
	};
	
	  var readHTMLFile = function(url){
          var toReturn;
          $.ajax({
              url: url,
              async: false
          }).done(function(data){
              toReturn = data;
          });
          return toReturn;
      };
	// --------------------------------------------

	// --------------------------------------------
	// Code Mirror Editor
	// Define an extended mixed-mode that understands vbscript and
	// leaves mustache/handlebars embedded templates in html mode
	// --------------------------------------------
	var mixedMode = {
		name : "htmlmixed",
		scriptTypes : [ {
			matches : /\/x-handlebars-template|\/x-mustache/i
		} ]
	};

	var javascriptMode = {
		name : "javascript"
	};

//	var editorHtml = CodeMirror.fromTextArea(document
//			.getElementById("template_content"), {
//		mode : mixedMode,
//		tabMode : "indent",
//		lineWrapping : true,
//		lineNumbers : true,
//		onKeyEvent : function(e, s) {
//			if (s.type == "keyup") {
//				forceSaveTemplate()
//				renderEdit()
//			}
//		}
//	});

//	var editorJavascript = CodeMirror.fromTextArea(document
//			.getElementById("template_scripting"), {
//		mode : javascriptMode,
//		tabMode : "indent",
//		lineWrapping : true,
//		lineNumbers : true,
//		matchBrackets : true,
//		onKeyEvent : function(e, s) {
//			if (s.type == "keyup") {
//				forceSaveTemplate()
//				renderEdit()
//			}
//		}
//	});

//	$("#show-codeblock").click(function(e) {
//		var template1 = $(".code-block");
//		if (!e.target.checked) {
//			template1.fadeOut();
//		} else {
//			template1.fadeIn();
//		}
//	})

	// $('#template-code-block').find('.CodeMirror').resizable({
	// resize : function() {
	// editorHtml.setSize($(this).width(), $(this).height());
	// }
	// });
	//
	// $('#scripting-code-block').find('.CodeMirror').resizable({
	// resize : function() {
	// editorJavascript.setSize($(this).width(), $(this).height());
	// }
	// });
	// --------------------------------------------

	// --------------------------------------------
	// Context Menu Configuration
	// --------------------------------------------
	
	function initContextMenu() {
		$.contextMenu({
			selector : 'svg',
			items : {
				"edit" : {
					name : "Edit SVG",
					icon: "edit",
					callback : function(key, options) {
						openSvgEditor(this)
					}
				}
			}
		});
	}
	
	function disableContextMenu() {
		$.contextMenu( 'destroy' );
	}
	
	function openSvgEditor(e) {
		var svgElement = $(e);
		var svgstring = $(e).toHtmlString()
	    $("#svgModal").on('hidden', function() {
        	saveSvg(svgElement)
        });
		$("#svgModal").modal('show');
   		svgCanvas.setSvgString(svgstring);
	}

	function saveSvg(svgElement) {
		svgCanvas.getSvgString()(function(data, error) {
			if (error) {
				alert(error);
			} else {
				if (svgElement) {
					svgElement.replaceWith($(data))
					editorHtml.setValue($("#result").children().toHtmlString())
					forceSaveTemplate()
//					renderEdit()
				}
			}
		});
	}

	$("#svgedit").load(function() {
		var frame = document.getElementById('svgedit');
		svgCanvas = new embedded_svg_edit(frame);
	});
	
	// --------------------------------------------

	// --------------------------------------------
	// Calls to Server
	// --------------------------------------------
	function forceRendering() {
		session.sendCmd("forcerendering", {
			"client" : extern.client
		})
	}

	function forceSaveTemplate() {
		session.sendCmd("saveTemplate", {
			"template_content" : editorHtml.getValue(),
			"template_scripting" : editorJavascript.getValue(),
			"client" : extern.client
		})
	}
	// --------------------------------------------

	// --------------------------------------------
	// Rendering
	// --------------------------------------------
	function renderEdit() {
		
		if (template != null) {
			try {
				var newIframeHeight = $("#iframeTemplate").contents()
						.find("html").height()
						+ 'px';
				$('#iframeTemplate').css("height", newIframeHeight);
			} catch (e) {
				template_error(e)
			}
		}		
		initDnd()
		
	}
	
//	function bmsrender(d) {
//		var s;
//		$.ajax({
//			url : "/bms/lift.html?json="+JSON.stringify(d),
//			type: 'GET',
//	        dataType: 'json',
//	        data: JSON.stringify(d),
//	        contentType: 'application/json',
//	        mimeType: 'application/text',
//			success : function(result) {
//				if (result.isOk === false) {
//					alert(result.message);
//				} else {
//					console.log(result)
//					s = result;
//					console.log(s)
//				}
//			},
//			async : false
//		});
//		return s;
//	}
	

	function renderVisualization(data) {

		if(templateFile) {
//			var renderedTemplate = readHTMLFile("http://localhost:8080/bms/"
//					+ templateFile + "?json=" + data)
//			console.log(renderedTemplate)
//			var jsonp = JSON.parse(data)
//			var concatjson = jQuery.extend({}, jsonp, extern.observer);
//			renderedTemplate = Mustache.render(renderedTemplate, concatjson)
//			var body = renderedTemplate.replace(/^[\S\s]*<body[^>]*?>/i, "")
//					.replace(/<\/body[\S\s]*$/i, "");
			
			var src = "http://localhost:8080/bms/" + templateFile + "?json=" + data;
			
			$("#result_container").append('<iframe src="" width="100%" frameborder="0" scrolling="no" name="tempiframe" id="tempiframe" style="visibility:hidden"></iframe>')
						
			$('#tempiframe').attr("src", src)
							
			$('#tempiframe').on('load', function() {
				$('#tempiframe').css("visibility","visible")
				$('#iframeVisualization').remove()
				$('#tempiframe').attr("id","iframeVisualization")
				resizeIframe()
			});	
			
	//		var head = renderedTemplate.replace(/^[\S\s]*<head[^>]*?>/i, "")
	//				.replace(/<\/head[\S\s]*$/i, "");
	//		$('#iframeVisualization').contents().find('head').html(head)
//			$('#iframeVisualization').contents().find('body').html(body)	
		}
		
	}

	function initDnd() {
		$('.draggable').draggable({
			stop : function(e, ui) {
				editorHtml.setValue($("#result").children().toHtmlString())
			}
		});
	}
		
	// --------------------------------------------

	function restoreTemplate(template_content, template_scripting) {
		if (template_content != null)
			editorHtml.setValue(template_content);
		if (template_scripting != null)
			editorJavascript.setValue(template_scripting);
	}

	function restoreFormulas(formulas) {
		var id, formula, idNum;
		for ( var i = formulas.length - 1; i >= 0; i--) {
			id = formulas[i].id;
			formula = formulas[i].formula;
			$("#formulas").prepend(
					session.render("/ui/bmsview/formula_entered.html", {
						id : id,
						formula : formula
					}));
		}
		hookEnteredFieldListener()
	}
	
	function restoreObserver(observer) {
		var observerList = $('#svgedit').contents().find('#observer_list')
		for ( var i = 0; i <= observer.length - 1; i++) {
			var o = observer[i];
			var odata = JSON.parse(o.data)
			observerList.append(
			"<h3>New Observer</h3><div>" + 
			session.render("/ui/bmsview/observer/predicateObserverForm.html", odata) +
			"</div>")
		}
	}

	extern.client = ""
	extern.observer = null;
	extern.workspace = "";
	extern.init = session.init
	extern.session = session

	extern.renderVisualization = function(data) {
		renderVisualization(data.data)
	}

	extern.restorePage = function(data) {
		restoreFormulas(JSON.parse(data.formulas))
		restoreTemplate(data.template_content, data.template_scripting)
		forceRendering()
	}

	extern.register = function(observer, expression) {
		session.sendCmd("register", {
			"observer" : observer,
			"expression" : expression,
			"client" : extern.client
		})
	}

	extern.formulaRemoved = function(data) {
		formulaRemoved(data.id);
	}

	extern.openSvgEditor = function(e) {
		openSvgEditor(e)
	}

	// --------------------------------------------
	// Observer / Formulas
	// --------------------------------------------
	function appendNewInputField(nextId) {
		$("#formulas").append(session.render("/ui/bmsview/input_field.html", {
			id : nextId,
			value : "",
			text : "Add"
		}));
	}

	function replaceWithEnteredField(id, formula) {
		$("#" + id).removeClass("has-error");
		$("#input-" + id).replaceWith(
				session.render("/ui/bmsview/formula_entered.html", {
					id : id,
					formula : formula
				}));
	}

	function replaceWithEditField(id, formula) {
		var parentId = "#input-" + id;
		$(parentId).replaceWith(
				session.render("/ui/bmsview/input_field.html", {
					id : id,
					value : formula,
					text : "Ok"
				}));
	}

	function formulaAdded(id, formula, nextId) {

		// Append a new input field ...
		appendNewInputField(nextId);
		// ... and hook corresponding listeners
		hookInputFieldListener(true);

		// Replace formula field and ...
		replaceWithEnteredField(id, formula);
		// ... hook listener for edit and remove buttons
		hookEnteredFieldListener();

	}

	function formulaRemoved(id) {
		$("#input-" + id).remove();
	}

	function hookEnteredFieldListener() {

		$("[id^=edit-]").unbind('click');
		$("[id^=remove-]").unbind('click');

		$("[id^=edit-]").click(function(e) {
			e.preventDefault();
			var id = e.target.parentElement.id;
			var formula = $("#formula-" + id)[0].textContent;
			editFormula(id, formula);
		});
		$("[id^=remove-]").click(function(e) {
			e.preventDefault();
			var id = e.target.parentElement.id;
			session.sendCmd("removeFormula", {
				"id" : id
			});
		})

	}

	function hookInputFieldListener(newformula) {

		$(".add-formula").unbind('click');
		$(".form-control").unbind('keyup');

		$(".add-formula").click(function(e) {
			e.preventDefault();
			var id = e.target.parentNode.parentNode.id;
			session.sendCmd("addFormula", {
				"id" : id,
				"newFormula" : newformula
			});
		});

		$(".form-control").keyup(function(e) {
			session.sendCmd("parse", {
				"formula" : e.target.value,
				"id" : e.target.parentNode.id
			})
		});

	}

	function editFormula(id, formula) {
		replaceWithEditField(id, formula);
		hookInputFieldListener(false);
	}

	function formulaRestored(id, formula) {
		replaceWithEnteredField(id, formula);
		hookEnteredFieldListener();
//		renderEdit()
	}
	
	function parseOk(id) {
		$("#" + id).removeClass("has-error")
		$("#btn-" + id).prop("disabled", false);
	}

	function parseError(id) {
		$("#" + id).addClass("has-error")
		$("#btn-" + id).prop("disabled", true);
	}
	
	function browse(dir_dom) {
		$('#filedialog').off('hidden.bs.modal')
		$('#filedialog').on('hidden.bs.modal',
				set_ok_button_state(dir_dom))
		$("#filedialog").modal('show')
		browse2(dir_dom)
	}
	
	function set_ok_button_state(dir_dom) {
		return function() {
			var file = $(dir_dom)[0].value
			var valid = check_file(file);
			if (valid) {
				$("#fb_okbtn").removeAttr("disabled")
			} else {
				$("#fb_okbtn").attr("disabled", "disabled")
			}
		}
	}
	
	function browse2(dir_dom) {
		var dir = $(dir_dom)[0].value
		// prepare dialog
		var data = request_files(dir)
		$(dir_dom).val(data.path)
		filldialog(data.dirs, data.files, dir_dom)
	}
	
	function request_files(d) {
		var s;
		$.ajax({
			url : "/files?path=" + d + "&extensions=bms&workspace="+extern.workspace,
			success : function(result) {
				if (result.isOk === false) {
					alert(result.message);
				} else {
					s = JSON.parse(result);
				}
			},
			async : false
		});
		return s;
	}
	
	function check_file(d) {
		var s;
		$.ajax({
			url : "/files?check=true&path=" + d + "&extensions=bms&workspace="+extern.workspace,
			success : function(result) {
				if (result.isOk === false) {
					alert(result.message);
				} else {
					s = JSON.parse(result);
				}
			},
			async : false
		});
		return s;
	}

	function filldialog(dirs, files, dir_dom) {
		$(".filedialog_item").remove()
		$(".filedialog_br").remove()
		var hook = $("#filedialog_content")
		var s
		for (s in dirs) {
			var file = dirs[s]
			if (!file.hidden) {
				hook.append(session.render("/ui/bmsview/fb_dir_entry.html", {
					"name" : file.name,
					"path" : file.path,
					"dom" : dir_dom
				}))
			}
		}
		for (s in files) {
			var file = files[s]
			if (!file.hidden) {
				hook.append(session.render("/ui/bmsview/fb_file_entry.html",
						{
							"name" : file.name,
							"path" : file.path,
							"dom" : dir_dom
						}))
			}
		}
	}
	
	function fb_select_dir(dir_dom, path) {
		$(dir_dom).val(path)
		browse2(dir_dom)
	}
	function fb_select_file(dir_dom, path) {
		$(dir_dom).val(path)
		$("#filedialog").modal('hide')
	}
	

	function resizeIframe() {
		var newIframeHeight = $("#iframeVisualization").contents().find("html")
				.height()
				+ 'px';
		$('#iframeVisualization').css("height", newIframeHeight);
	}
	
	extern.browse = browse
	extern.fb_select_dir = fb_select_dir
	extern.fb_select_file = fb_select_file
	extern.fb_load_file = function(dom_dir) {
		session.sendCmd("setTemplate", {
			"path" : $(dom_dir)[0].value
		})
		focused = null; // prevent blur event
		$("#sourceModal").modal('hide')
		$("#chooseTemplateBox").css("display","none");
	}
	
	extern.setTemplate = function(data) {
		templateFile = data.templatefile
		if(templateFile) {
			$('#iframeVisualization').attr(
					"src",
					"http://localhost:8080/bms/" + templateFile + "?json="
							+ data.data)
			$('#iframeVisualization').load(function(){
				resizeIframe();
//				forceRendering();
			});
			
		}
	}
	
	extern.parseError = function(data) {
		parseError(data.id);
	}
	extern.parseOk = function(data) {
		parseOk(data.id);
	}

	extern.formulaAdded = function(data) {
		formulaAdded(data.id, data.formula, data.nextId);
	}
	
	extern.formulaRestored = function(data) {
		formulaRestored(data.id, data.formula);
	}
	
	extern.restoreObserver = function(observer) {
		restoreObserver(JSON.parse(observer.data))
	}	
	
	// --------------------------------------------
	
	// --------------------------------------------
	// Error Handling
	// --------------------------------------------
	extern.error = function(data) {
		error(data.id, data);
	}

	function error(id, errormsg) {
		$("#right-col").prepend(
				session.render("/ui/bmsview/error_msg.html", errormsg));
		$("#" + id).addClass("has-error");
	}
	// --------------------------------------------
	
	// --------------------------------------------
	// External API Calls
	// --------------------------------------------
	extern.executeOperation = function(op, predicate) {
		session.sendCmd("executeOperation", {
			"op" : op,
			"predicate" : predicate,
			"client" : extern.client
		})
	}
	// --------------------------------------------
		
	return extern;

}())
