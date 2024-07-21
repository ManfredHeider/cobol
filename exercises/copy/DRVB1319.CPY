      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1319                                      *
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
      *    FÜR DIE FUNKTION DRVB1319                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1319.
          05 PI-DRVB1319-PROGRAMM           PIC  X(008).
          05 PI-DRVB1319-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1319-DAT-VON            PIC  X(010).
          05 PI-DRVB1319-DAT-BIS            PIC  X(010).
          05 PI-DRVB1319-KNR-VERTRIEB-GES   PIC S9(004) BINARY.
      *
       01 PO-DRVB1319.
          05 PO-DRVB1319-RC                 PIC  X(003).
             88 PO-DRVB1319-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1319-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1319-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1319-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1319-RC-ERR                       VALUE 'ERR'.
          05 PO-DRVB1319-DATEN.
             10 PO-DRVB1319-VBNR            PIC  S9(009)    COMP.
             10 PO-DRVB1319-MS-NR           PIC  S9(004)    COMP.
             10 PO-DRVB1319-EH-GS-XX-EG     PIC  S9(009)V99 COMP-3.
      *
      ***** Ende-Copy-Member *****************************************
