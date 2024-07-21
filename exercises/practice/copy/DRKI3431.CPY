      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3431.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3431.CPv  $
      *
      *   Rev 2.5   May 10 2019 12:55:28   t13ae52
      *AKI01680: G42: Erweiterung Datenspiegel
      *
      *   Rev 2.4   Nov 22 2018 10:00:40   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.3   Nov 05 2018 15:47:54   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.2   Oct 27 2018 15:16:02   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.1   Oct 25 2018 14:37:00   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.0   Sep 07 2018 11:02:42   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3431                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0512 G42_DEPOTSTAMM                 *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!13.09.18!2.1 ! Neuerstellung             ! tfl      *
      * -------------------------------------------------------------*
      * AKI01680!09.05.19!2.5 !NEUES FELD                 !SCHUMACHER*
      *         !        !    !G42DWPHGAKTUDAT            !          *
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3431
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3431.
           05 PI-DRKI3431-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3431-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3431-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3431-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3431-KEY.
               10 PI-DRKI3431-G42DMANDANT     PIC S9(009) COMP.
 
      *
       01  PO-DRKI3431.
           05 PO-DRKI3431-RC                   PIC  X(003).
              88 PO-DRKI3431-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3431-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3431-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3431-DATEN.
              10 PO-DRKI3431-G42DVERTRAGSNR  PIC  X(0015).
              10 PO-DRKI3431-G42DANLAGEZIEL   PIC  X(0001).
              10 PO-DRKI3431-G42DBIC   PIC  X(0011).
              10 PO-DRKI3431-G42DDEPOTART   PIC  X(0001).
              10 PO-DRKI3431-G42DIBAN   PIC  X(0034).
              10 PO-DRKI3431-G42DKONTOINHABER   PIC  X(0030).
              10 PO-DRKI3431-G42DMANDREF   PIC  X(0035).
              10 PO-DRKI3431-G42DNRGWBBAVH   PIC S9(6)V9(2) COMP-3.
              10 PO-DRKI3431-G42DNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3431-G42DNRGWOBBAVH   PIC S9(6)V9(2) COMP-3.
              10 PO-DRKI3431-G42DNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3431-G42DOEFFNUNGSDAT   PIC  X(0008).
              10 PO-DRKI3431-G42DONLINEKZ   PIC  X(0001).
              10 PO-DRKI3431-G42DRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
              10 PO-DRKI3431-G42DRISIKOKLASSE  PIC  X(0001).
              10 PO-DRKI3431-G42DSTATUS  PIC  X(0001).
              10 PO-DRKI3431-G42DWPHGAKTUDAT PIC X(0008).
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 


