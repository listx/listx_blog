$(function() {
	$("h1,h2,h3,h4,h5,h6").each(function() {
		var $this = $(this);
		var text = $this.text();
		var href = $this.attr("id");
		var href_parts = [];
		var href_custom = "";
		if (typeof href === "undefined") {
			href_custom = "";
			href_parts = text.split(" ");
			for (i = 0; i < href_parts.length; i++) {
				href_custom += href_parts[i] + '-';
			}
			href_custom = href_custom.slice(0, -1).toLowerCase();
			console.log(href_custom);
		}
		if (href !== "page-title") {
			$this.text("");
			if (typeof href === "undefined") {
				href = href_custom;
				$this.attr("id", href);
			}
			$this.prepend($("<a>").attr("href", '#' +
				href).addClass("section-heading").text(text));
		}
	});
});
