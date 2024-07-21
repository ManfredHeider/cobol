      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1318                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Th. Willig                                       *
      *    DATUM:    01.09.2021                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-3230!01.09.21! 01 ! NEUERSTELLUNG             ! T13AE30  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1318                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1318.
          05 PI-DRVB1318-PROGRAMM           PIC  X(008).
          05 PI-DRVB1318-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1318-MS-NR              PIC  9(004).
          05 PI-DRVB1318-DAT-VON            PIC  X(010).
          05 PI-DRVB1318-DAT-BIS            PIC  X(010).
          05 PI-DRVB1318-ED-STUFE-AB        PIC  9(004) COMP.
          05 PI-DRVB1318-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1318.
          05 PO-DRVB1318-RC                 PIC  X(003).
             88 PO-DRVB1318-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1318-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1318-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1318-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1318-RC-ERR                       VALUE 'ERR'.
          05 PO-DRVB1318-DATEN.
             10 PO-DRVB1318-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1318-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1318-EH-GS-XX-EG     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
