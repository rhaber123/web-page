<xsl:stylesheet version = '1.0'
     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:template match="BBB">
     <div style="color:purple">
          <xsl:value-of select="name()"/>
          <xsl:text> id=</xsl:text>
          <xsl:value-of select="@id"/>
     </div>
</xsl:template>

<xsl:template match="/source/AAA/CCC/DDD">
     <p style="color:red">
          <xsl:value-of select="name()"/>
          <xsl:text> id=</xsl:text>
          <xsl:value-of select="@id"/>
     </p>
</xsl:template>

</xsl:stylesheet>