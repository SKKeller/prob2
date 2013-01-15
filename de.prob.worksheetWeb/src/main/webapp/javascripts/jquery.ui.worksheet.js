(function($, undefined) {

	$.widget("ui.worksheet", {
		version : "0.1.0",
		options : {
			jsUrls : [ "javascripts/libs/jquery-ui-1.9.2/ui/jquery.ui.menubar.js" ],
			cssUrls : [ "stylesheets/jquery-ui-1.9.2/themes/base/jquery.ui.menubar.css" ],
			initialized : function(event) {
				window.console.debug("Event: initialized from worksheet");
			},
			optionsChanged : function(event, options) {
				window.console.debug("Event: optionsChanged from worksheet: " + options.id);
			},
		},
		blocks : [],
		_create : function() {
			$("body").lazyLoader();
			$("body").lazyLoader("loadStyles", this.options.cssUrls);
			$("body").one("scriptsLoaded", 0, $.proxy(this._create2, this));
			$("body").lazyLoader("loadScripts", this.options.jsUrls);
		},
		_create2 : function() {
			this.element.uniqueId().addClass("ui-worksheet ui-widget ui-corner-none");

			this.options.id = this.element.attr("id");

			if (this.options.hasMenu) {
				var worksheetMenu = null;
				if (this.options.menu.length > 0) {
					worksheetMenu = this.createULFromNodeArrayRecursive(this.options.menu);
				} else {
					worksheetMenu = $("<ul></ul>");
				}
				worksheetMenu.addClass("ui-worksheet-menu").menubar();
				this.element.append(worksheetMenu);

			}
			if (this.options.hasBody) {
				var worksheetBody = $("<div></div>").addClass("ui-worksheet-body ui-widget-content ");
				this.element.append(worksheetBody);
				worksheetBody.sortable({
					items : "> .ui-block",
					handle : ".ui-sort-handle",
					update : function(event, ui) {
						$("#ui-id-1").worksheet("moveInBlocks", parseInt(ui.item.attr("tabindex")) - 1, ui.item.prevAll().length);
					},
				});

				if (this.options.blocks.length > 0) {
					var blockOptions = this.options.blocks;
					this.options.blocks = [];
					for ( var x = 0; x < blockOptions.length; x++) {
						this.appendBlock(blockOptions[x]);
					}
				}
			}
			
			this._trigger("initialized",0,[]);
			this._trigger("optionsChanged",0,[this.options]);
		},
		createULFromNodeArrayRecursive : function(nodes) {
			var menu = $("<ul></ul>");
			for ( var x = 0; x < nodes.length; x++) {
				var menuItem = this.nodeToUL(nodes[x]);
				if (nodes[x].children.length > 0) {
					menuItem.append(this.createULFromNodeArrayRecursive(nodes[x].children));
				}
				menu.append(menuItem);
			}
			return menu;
		},
		nodeToUL : function(node) {
			var nodeItem = $("<li></li>");
			if (node.itemClass != "") {
				nodeItem.addClass(node.itemClass);
			}
			var item = $("<a></a>");

			if (node.iconClass != "") {
				var icon = $("<span></span>").addClass("ui-icon " + node.iconClass);
				item.append(icon);
			}
			if (node.text != "") {
				item.append(node.text);
			}
			item.click(node.click);
			nodeItem.append(item);
			return nodeItem;
		},
		appendBlock : function(blockOptions) {
			var index = this.options.blocks.length;
			return this.insertBlock(blockOptions, index);
		},
		insertBlock : function(blockOptions,index){
			// set options for block if needed
			if (blockOptions != null && blockOptions != {}){
				blockOptions.worksheetId = this.options.id;
				this.insertIntoBlocks(index, blockOptions);
			}
			var block = $("<div></div>");
			if (this.options.blocks.length > 1) {
				$(this.element.find(".ui-worksheet-body>.ui-block")[index - 1]).after(block);
			} else {
				this.element.find(".ui-worksheet-body").append(block);
			}
			block.block(this.options.blocks[index]);
			block.bind("blockoptionschanged", $.proxy(function(event, options) {
				var index=this.getBlockIndexById(options.id);
				this.options.blocks[index]=options;
				this._trigger("optionsChanged", 0, [ this.options ]);
			}, this));
			block.attr("tabindex",index+1);
			this.element.find(".ui-worksheet-body").sortable("refresh");
			this._trigger("optionsChanged",0,this.options);
		},
		insertIntoBlocks : function(index, element) {
			this.options.blocks.push(null);
			for ( var x = this.options.blocks.length - 1; x > index; x--) {
				this.options.blocks[x] = this.options.blocks[x - 1];
			}
			this.options.blocks[x] = element;
			$("#" + element.id).attr("tabindex", index + 1);
		},
		removeFromBlocks : function(index) {
			for ( var x = index; x < this.options.blocks.length - 1; x++) {
				this.options.blocks[x] = this.options.blocks[x + 1];
			}
			this.options.blocks.pop();
		},
		moveInBlocks : function(indexFrom, indexTo) {
			var temp = this.options.blocks[indexTo];
			this.options.blocks[indexTo] = this.options.blocks[indexFrom];
			this.options.blocks[indexFrom] = temp;
			$("#" + this.options.blocks[indexTo].id).attr("tabindex", indexTo + 1);
			$("#" + this.options.blocks[indexFrom].id).attr("tabindex", indexFrom + 1);
			// TODO inform server about position change
			// TODO call eval (following)
		},
		removeBlock : function(index) {
			var block=$("#" + this.options.blocks[index].id);
			block.block("destroy");
			block.remove();
			for ( var x = index; x < this.options.blocks.length - 1; x++) {
				this.options.blocks[x] = this.options.blocks[x + 1];
			}
			this.options.blocks.pop();
			this.element.find(".ui-worksheet-body").sortable("refresh");
		},
		getBlockIndexById : function(id) {
			for ( var x = 0; x < this.options.blocks.length; x++) {
				if (this.options.blocks[x].id == id)
					break;
			}
			return x;
		},
		removeBlockById : function(id) {
			var index = this.getBlockIndexById(id);
			this.removeBlock(index);
		},
		insertMenu : function(menu) {
			this.element.children(".ui-worksheet-menu").empty().append(menu);
		},
		isLastBlock : function(blockId) {
			return this.blocks[this.blocks.length - 1].block("option", "id") == blockId;
		},
		getBlocks : function() {
			return this.blocks;
		},
		_setOption :function(key,val){
			this._super( "_setOption", key, value );
			this._trigger("optionsChanged",0,this.options);
		},
		evaluate:function(blockId){
			var msg=this.options.blocks[this.getBlockIndexById(blockId)];
            delete msg.menu;
            
			var content = this._addParameter("", "block", $.toJSON(msg));
			content = this._addParameter(content,"worksheetSessionId", "1");
			$.ajax("setBlock", {
				type : "POST",
				data : content
			}).done($.proxy(function(data, status, xhr) {
				var content = this._addParameter("", "id", blockId);
	            content = this._addParameter(content, "worksheetSessionId", "1");
	            $.ajax("evaluate", {
					type : "POST",
					data : content
				}).done($.proxy(function(data, status, xhr) {
					var text=xhr.responseText;
					data = (new Function( "return " + text))();
					data = $.recursiveFunctionTest(data);
					
					var index=this.getBlockIndexById(data[0].id);
					for(var x=this.options.blocks.length-1;x>=index;x--){
						this.removeBlock(x);
					}

					for ( var x = 0; x < data.length; x++) {
						 this.appendBlock(data[x]);
					}
				},this));
			},this));
		},
		_addParameter : function(res, key, value) {
			if (res != "")
				res += "&";
			res += encodeURIComponent(key) + "="
					+ encodeURIComponent(value);
			return res;
		},

	});
}(jQuery));
