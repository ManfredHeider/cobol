      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  10.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3458.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3458.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:43:12   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.2   Oct 27 2018 15:26:14   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.1   Sep 19 2018 11:48:26   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.0   Sep 10 2018 15:49:02   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3458                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0511 G79_FSA                        *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!13.09.18!2.1 ! Neuerstellung             ! tfl      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3458
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3458.
           05 PI-DRKI3458-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3458-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3458-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3458-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3458-KEY.
               10 PI-DRKI3458-G42FMANDANT      PIC S9(009) COMP.
 
      *
       01  PO-DRKI3458.
           05 PO-DRKI3458-RC                   PIC  X(003).
              88 PO-DRKI3458-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3458-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3458-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3458-DATEN.
              10 PO-DRKI3458-G42FVERTRAGSNR   PIC  X(0015).
              10 PO-DRKI3458-G42FAUFTKONF1   PIC S9(3)V COMP-3.
              10 PO-DRKI3458-G42FAUFTKONF2   PIC S9(3)V COMP-3.
              10 PO-DRKI3458-G42FAUSLQSTEUER   PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3458-G42FFBETRVERBR   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3458-G42FFSAID   PIC  X(0008).
              10 PO-DRKI3458-G42FGEMFBETRAG   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3458-G42FGUELTIGAB   PIC  X(0008).
              10 PO-DRKI3458-G42FGUELTIGBIS   PIC  X(0008).
              10 PO-DRKI3458-G42FKISTEBEHALT   PIC  X(0008).
              10 PO-DRKI3458-G42FKISTSATZ1   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3458-G42FKISTSATZ2   PIC S9(2)V9(2) COMP-3.
              10 PO-DRKI3458-G42FKONFESSION1   PIC  X(0002).
              10 PO-DRKI3458-G42FKONFESSION2   PIC  X(0002).
              10 PO-DRKI3458-G42FKZSTAUSL   PIC  X(0001).
              10 PO-DRKI3458-G42FKZVERBAKTIE   PIC  X(0001).
              10 PO-DRKI3458-G42FKZVERBSONST   PIC  X(0001).
              10 PO-DRKI3458-G42FNVGUELTIGBIS   PIC  X(0008).
              10 PO-DRKI3458-G42FSTUECKZINS   PIC S9(10)V9(2) COMP-3.
              10 PO-DRKI3458-G42FSUMERTRAG   PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3458-G42FSUMKEST   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3458-G42FSUMKIST   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3458-G42FSUMSOLI   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3458-G42FVERBVORJ   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3458-G42FVERLUSTAKTI   PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3458-G42FVERLUSTSON   PIC S9(7)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 

