      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0464.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0464.CPv  $
      *
      *   Rev 2.2   Mar 20 2002 14:20:28   t13ae35
      *FSL00091 - Betragsfeld mit Vorzeichen definiert
      *
      *   Rev 2.1   Apr 26 2000 12:38:18   T13AE35
      *ASL00049 - neu, ANSPRUECHE AN DIM7-OLB-LEISTUNGEN 
      *JE QUARTAL
      *
      *   Rev 2.0   Apr 03 2000 14:10:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0464                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0076                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    11.04.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00049!11.04.00! 01 ! NEUERSTELLUNG             ! Walb     *
      * -------------------------------------------------------------*
      * FSL00091!20.03.02! 02 ! Betrag mit VZ definiert   ! Hering   *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0464                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0464.
          05 PI-DRVB0464-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0464-MONAT              PIC  9(002).
          05 PI-DRVB0464-JAHR               PIC  9(004).
      *
       01 PO-DRVB0464.

          05 PO-DRVB0464-RC                 PIC  X(003).
             88 PO-DRVB0464-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0464-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0464-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0464-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0464-DATEN.
             10 PO-DRVB0464-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0464-KNR-OLZ-TYP     PIC S9(004)  COMP.
             10 PO-DRVB0464-KTY-OLZ-TYP     PIC S9(004)  COMP.
             10 PO-DRVB0464-KNR-WAEHRUNG    PIC S9(004)  COMP.
             10 PO-DRVB0464-KTY-WAEHRUNG    PIC S9(004)  COMP.
200203*      10 PO-DRVB0464-OBAZ-BETRAG     PIC 9(011).
200203       10 PO-DRVB0464-OBAZ-BETRAG     PIC S9(011).
      *
      ***** ENDE COPY-MEMBER DRVB0464 *****************************
