<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="list">
     <xsl:for-each select="entry">
          <xsl:value-of select="@name"/>
          <xsl:text>, </xsl:text>
     </xsl:for-each>
</xsl:template>


</xsl:stylesheet>