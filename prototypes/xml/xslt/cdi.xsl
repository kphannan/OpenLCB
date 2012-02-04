<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- $Id: throttle.xsl 19667 2012-01-07 23:33:11Z jacobsen $ -->

<!-- Stylesheet to convert an OpenLCB CDI document into displayable HTML    -->

<!-- This file is part of OpenLCB.  Copyright 2011.                         -->
<!--                                                                        -->
 
<xsl:stylesheet	version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Need to instruct the XSLT processor to use HTML output rules.
     See http://www.w3.org/TR/xslt#output for more details
-->
<xsl:output method="html" encoding="ISO-8859-1"/>


<!-- This first template matches our root element in the input file.
     This will trigger the generation of the HTML skeleton document.
     In between we let the processor recursively process any contained
     elements, which is what the apply-templates instruction does.
     We can also pick some stuff out explicitly in the head section using
     value-of instructions.
-->     
<xsl:template match='cdi'>

<html>
	<head>
		<title>OpenLCB CDI File</title>
	</head>
	
	<body>
		<h2>OpenLCB CDI File</h2>

                <xsl:apply-templates/>

<HR/>
This page was produced by <a href="http://jmri.org">OpenLCB</a>.
<P/>Copyright &#169; 2012 OpenLCB Community. 
<P/>OpenLCB and associated logos are our trademarks.
<P/><A href="http://openlcb.org/Copyright.html">Additional information on copyright, trademarks and licenses is linked here.</A>

	</body>
</html>

</xsl:template>

<!-- Identification Block -->
<xsl:template match="identification">
<h3>Identification Block
<xsl:apply-templates/>
</h3>
</xsl:template>

<xsl:template match="identification/manufacturer">
<h4>Manufacturer: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="identification/model">
<h4>Model: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="identification/hardwareVersion">
<h4>Hardware Version: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="identification/softwareVersion">
<h4>Software Version: <xsl:apply-templates/></h4>
</xsl:template>

<!-- General map support -->
<xsl:template match="map"><table border="1"><xsl:apply-templates/></table></xsl:template>
<xsl:template match="relation"><tr><xsl:apply-templates/></tr></xsl:template>
<xsl:template match="property"><th><xsl:apply-templates/></th></xsl:template>
<xsl:template match="value"><td><xsl:apply-templates/></td></xsl:template>

<!-- Segment -->
<!-- needs conditional handling of origin, space attribute presense (or default values?) -->
<xsl:template match="segment">
<h3>Segment at origin <xsl:value-of select="@origin"/> in space <xsl:value-of select="@space"/></h3>
<xsl:apply-templates/>
</xsl:template>

<!-- Group -->
<!-- needs conditional handling of offset, replication attribute presense (or default values?) -->
<xsl:template match="group">
<h4>Group at offset <xsl:value-of select="@offset"/> replication count <xsl:value-of select="@replication"/></h4>
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="eventid">EventID: <blockquote><xsl:apply-templates/></blockquote><br/></xsl:template>
<xsl:template match="bit">Bit: <blockquote><xsl:apply-templates/></blockquote><br/></xsl:template>
<xsl:template match="int">Int: <blockquote><xsl:apply-templates/></blockquote><br/></xsl:template>

<xsl:template match="name">Name: <xsl:apply-templates/><br/></xsl:template>
<xsl:template match="description">Description: <xsl:apply-templates/><br/></xsl:template>
<xsl:template match="min">Min: <xsl:apply-templates/><br/></xsl:template>
<xsl:template match="max">Max: <xsl:apply-templates/><br/></xsl:template>
<xsl:template match="default">Default: <xsl:apply-templates/><br/></xsl:template>


</xsl:stylesheet>
