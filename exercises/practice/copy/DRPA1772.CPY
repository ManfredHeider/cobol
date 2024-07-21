      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1772.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1772.CPv  $
      *
      *   Rev 2.2   Apr 07 2016 10:14:38   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.1   Jan 13 2014 07:22:36   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.0   Oct 02 2013 07:46:34   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1772                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1772                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                   *
      *    DATUM:    01.10.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01539!01.10.13!2.01! NEU                       ! STORCH   *
      * APA02220!10.03.16!2.02! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1772
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1772 DES BUFFER-OZP ZYPA0027 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PRODUKTUPRO TABELLE PRKO
      *
      ****************************************************************
      *
       01  PI-DRPA1772.
           05 PI-DRPA1772-UFKT                PIC  9(02).
           05 PI-DRPA1772-LOG-SATZ-NR         PIC  9(05).
      *
       01  PO-DRPA1772.
           05 PO-DRPA1772-RC                  PIC  X(03).
              88 PO-DRPA1772-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1772-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA1772-RC-LEER                      VALUE 'LER'.
           05 PO-DRPA1772-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05 PO-DRPA1772-ERGEBNISZEILE.
              10 PO-DRPA1772-PGEB-GES            PIC S9(004)  COMP.
              10 PO-DRPA1772-PROD-PROD-SCHL      PIC S9(004)  COMP.
              10 PO-DRPA1772-PRKL-TARSCHL-GRUP   PIC S9(009)  COMP.
              10 PO-DRPA1772-PRKL-TARSCHL        PIC S9(009)  COMP.
              10 PO-DRPA1772-KNR-TARIF-KZ        PIC S9(004)  COMP.
              10 PO-DRPA1772-PRKL-LZ-VON         PIC S9(004)  COMP.
              10 PO-DRPA1772-PRKL-LZ-BIS         PIC S9(004)  COMP.
              10 PO-DRPA1772-PRKL-GUELTIG-AB     PIC  X(010).
              10 PO-DRPA1772-PRKL-GUELTIG-BIS    PIC  X(010).
              10 PO-DRPA1772-PRKL-KOND-NR        PIC S9(004)  COMP.
100316*       10 PO-DRPA1772-PRKL-GRUND-PROV-MA  PIC S9(003)V999 COMP-3.
100316        10 PO-DRPA1772-PRKL-GRUND-PROV-MA PIC S9(004)V9(4) COMP-3.
100316*       10 PO-DRPA1772-PRKL-PROV-SATZ-VTF  PIC S9(003)V999 COMP-3.
100316        10 PO-DRPA1772-PRKL-PROV-SATZ-VTF PIC S9(004)V9(4) COMP-3.
100316*       10 PO-DRPA1772-PRKL-PROV-SATZ-VWF  PIC S9(003)V999 COMP-3.
100316        10 PO-DRPA1772-PRKL-PROV-SATZ-VWF PIC S9(004)V9(4) COMP-3.
              10 PO-DRPA1772-PRKL-KUERZUNG-ART   PIC  X(001).
              10 PO-DRPA1772-PRKL-KUERZUNG-AUF   PIC S9(004)  COMP.
              10 PO-DRPA1772-KNR-PVPT-KZ         PIC S9(004)  COMP.
              10 PO-DRPA1772-PRKL-PROV-MIN       PIC S9(009)  COMP.
              10 PO-DRPA1772-PRKL-PROV-MAX       PIC S9(009)  COMP.
              10 PO-DRPA1772-KNR-KOND-REGEL-GRP  PIC S9(004)  COMP.
              10 PO-DRPA1772-KNR-KOND-REGEL-GSP  PIC S9(004)  COMP.

      *   BEI ERWEITERUNG TSQ-LÄNGE IN ZYPA0027 ANPASSEN
      ******* ENDE COPY-MEMBER **************************************

