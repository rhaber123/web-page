<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="employee">
     <b>
          <xsl:value-of select="."/>
     </b>
</xsl:template>

<xsl:template match="surname">
     <i>
          <xsl:value-of select="."/>
     </i>
</xsl:template>


</xsl:stylesheet>