      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2335.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2335.CPv  $
      *
      *   Rev 2.1   Oct 26 2009 07:14:14   t13ex14
      *AKI00846: Erw. der Bestands-DTA Badenia: OZP-Objekte
      *
      *   Rev 2.0   Aug 14 2009 11:28:08   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2335                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2335                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.08.2009/15:38:04
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2335
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2335.
           05  PI-DRKI2335-KEY.


             10 PI-DRKI2335-BAJGUTHPRODLFDNR   PIC S9(4)  COMP.
             10 PI-DRKI2335-BAJJAHR   PIC S9(4)  COMP.
             10 PI-DRKI2335-BAJVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI2335.
           05  PO-DRKI2335-RC                 PIC  X(003).
               88 PO-DRKI2335-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2335-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2335-DATEN.


             10 PO-DRKI2335-BAJSALDWHNFOEKTO   PIC S9(7)V9(2)  COMP-3.
             10 PO-DRKI2335-N-BJSLDWHNFKT   PIC S9(4) COMP.
             10 PO-DRKI2335-BAJSUMAVWLEINZHL   PIC S9(7)V9(2)  COMP-3.
             10 PO-DRKI2335-N-BJSMVWLNZHL   PIC S9(4) COMP.
             10 PO-DRKI2335-BAJSUMFOEREINZHL   PIC S9(7)V9(2)  COMP-3.
             10 PO-DRKI2335-N-BJSMFRNZHL   PIC S9(4) COMP.
             10 PO-DRKI2335-BAJSUMFOERVORJHR   PIC S9(7)V9(2)  COMP-3.
             10 PO-DRKI2335-N-BJSMFRVRJHR   PIC S9(4) COMP.
             10 PO-DRKI2335-BAJVLSUMJAHR   PIC S9(7)V9(2)  COMP-3.
             10 PO-DRKI2335-NI-BAJVLSUMJAHR   PIC S9(4) COMP.
             10 PO-DRKI2335-BAJWOPAVORJHR   PIC S9(4)  COMP.
             10 PO-DRKI2335-NI-BAJWOPAVORJHR   PIC S9(4) COMP.
             10 PO-DRKI2335-BAJWOPAVORVORJHR   PIC S9(4)  COMP.
             10 PO-DRKI2335-N-BJWPVRVRJHR   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
