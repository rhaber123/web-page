<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:variable name="T" select="concat(//text[1],' - ',//text[2],' - ',//text[3])"/>
<xsl:template match="/">
     <P>
          <xsl:value-of select="$T"/>
     </P>
</xsl:template>


</xsl:stylesheet>