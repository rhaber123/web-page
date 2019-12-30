<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:variable name="A1">
     <xsl:copy-of select="//TABLE[1]"/>
</xsl:variable>
<xsl:variable name="A2">
     <xsl:copy-of select="//TABLE[2]"/>
</xsl:variable>
<xsl:template match="/">
     <xsl:copy-of select="$A2"/>
     <xsl:copy-of select="$A1"/>
     <xsl:copy-of select="$A2"/>
</xsl:template>


</xsl:stylesheet>