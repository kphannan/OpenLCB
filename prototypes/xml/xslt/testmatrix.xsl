<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- $Id: throttle.xsl 19667 2012-01-07 23:33:11Z jacobsen $ -->

<!-- Stylesheet to convert an OpenLCB Test Matrix document into displayable HTML    -->

<!-- This file is part of OpenLCB.  Copyright 2012.                         -->
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
<xsl:template match='TestMatrix'>

<html>
	<head>
		<title>OpenLCB Test Matrix File</title>
	</head>
	
	<body>
		<h2>OpenLCB Test Matrix File</h2>

                <xsl:apply-templates/>

<HR/>
This page was produced by <a href="http://jmri.org">OpenLCB</a>.
<P/>Copyright &#169; 2012 OpenLCB Community. 
<P/>OpenLCB and associated logos are our trademarks.
<P/><A href="http://openlcb.org/Copyright.html">Additional information on copyright, trademarks and licenses is linked here.</A>

	</body>
</html>

</xsl:template>

<xsl:template match="Test">
<h3>Test Definition
<xsl:apply-templates/>
</h3>
</xsl:template>

<xsl:template match="Name">
<h4>Name: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="Description">
<h4>Description: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="SpecDoc">
<h4>SpecDoc: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="Classname">
<h4>Classname: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="Enabled">
<h4>Enabled: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="TestObjective">
<h4>TestObjective: <xsl:apply-templates/></h4>
</xsl:template>

<xsl:template match="Objective">
<h5>Objective: <xsl:apply-templates/></h5>
</xsl:template>

<xsl:template match="Results">
<h5>Results: <xsl:apply-templates/></h5>
</xsl:template>

</xsl:stylesheet>
