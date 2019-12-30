<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="employee">
     <xsl:value-of select="."/>
     <xsl:text>[</xsl:text>
     <xsl:apply-templates select="@id"/>
     <xsl:text>]</xsl:text>
</xsl:template>

<xsl:template match="@id">
     <b>
          <i>
               <xsl:value-of select="."/>
          </i>
     </b>
</xsl:template>


</xsl:stylesheet>