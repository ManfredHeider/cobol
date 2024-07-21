      *****************************************************************
      *    COPY-MEMBER: DRKI3119
      *    --------------------------------------------------------
      *    FUNKTION LIST FÜR ZXKI0561
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3119
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10293  !23.08.22! NEU                          ! T13AC20  
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 PI-DRKI3119.
          05 PI-DRKI3119-UFKT-CODE            PIC  9(002).  
             88 PI-DRKI3119-UFKT-FIRST             VALUE 1.
             88 PI-DRKI3119-UFKT-NEXT              VALUE 2.
             88 PI-DRKI3119-UFKT-QUIT              VALUE 3.
          05 PI-DRKI3119-KEY.
             10 PI-DRKI3119-G21VVMANDANT      PIC S9(009) BINARY.
      *
       01 PO-DRKI3119.
          05 PO-DRKI3119-RC                   PIC  X(003).
             88 PO-DRKI3119-RC-OK                  VALUE 'OK '.
             88 PO-DRKI3119-RC-LEER                VALUE 'LER'.
             88 PO-DRKI3119-RC-EOF                 VALUE 'EOF'.
          05 PO-DRKI3119-DATEN.
             10 PO-DRKI3119-G21VVVNR          PIC  X(015).
             10 PO-DRKI3119-G21VVMANDANT      PIC S9(009) BINARY.
             10 PO-DRKI3119-G21VVVERARB-KZN   PIC  X(001).
             10 PO-DRKI3119-G21VVVBNR         PIC S9(009) BINARY.
             10 PO-DRKI3119-G21VVSPARTE       PIC  X(003).
             10 PO-DRKI3119-G21VVAENDKZN      PIC  X(001).
             10 PO-DRKI3119-G21VVVERTRZUSTAND PIC S9(004) BINARY.
             10 PO-DRKI3119-G21VVVERTRBEGINN  PIC  X(010).
             10 PO-DRKI3119-G21VVVERTRABLAUF  PIC  X(010).
             10 PO-DRKI3119-G21VVZAHLBEITRAG  PIC S9(009)V9(2) 
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVNJBEITRAG    PIC S9(009)V9(2)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVZAHLWEISE    PIC S9(004) BINARY.
             10 PO-DRKI3119-G21VVPRODUKT      PIC  X(050).
             10 PO-DRKI3119-G21VVTARIF        PIC  X(050).
             10 PO-DRKI3119-G21VVRENTENBEZUG  PIC  X(001).
             10 PO-DRKI3119-G21VVRENTEVORAUS  PIC S9(007)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVRENTEGARANT  PIC S9(007)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVRNTZAHLW     PIC S9(004) BINARY.
             10 PO-DRKI3119-G21VVLEISTUNG-KZN PIC  X(001).
             10 PO-DRKI3119-G21VVBERECH-DAT   PIC  X(010).
             10 PO-DRKI3119-G21VVERLFALL-SUM  PIC S9(009)V9(2)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVTODFALL-SUM  PIC S9(009)V9(2)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVRNTFAKTOR    PIC S9(005)V9(2)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVKAPABFIND    PIC S9(009)V9(2)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-G21VVFONDSGUTH    PIC S9(009)V9(2)
                                              PACKED-DECIMAL.
             10 PO-DRKI3119-LETZTERAENDERER   PIC  X(010).
             10 PO-DRKI3119-LETZTEAENDERUNG   PIC  X(026).
      *****************************************************************
      *---------------------- ENDE DRKI3119 --------------------------* 
      *****************************************************************