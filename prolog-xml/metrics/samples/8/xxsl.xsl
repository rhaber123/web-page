<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="firstName|surname">
     <div>
          <xsl:text>[template: </xsl:text>
          <xsl:value-of select="name()"/>
          <xsl:text> outputs </xsl:text>
          <xsl:apply-templates/>
          <xsl:text> ]</xsl:text>
     </div>
</xsl:template>


</xsl:stylesheet>