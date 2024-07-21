      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2337.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2337.CPv  $
      *
      *   Rev 2.1   Oct 26 2009 07:14:28   t13ex14
      *AKI00846: Erw. der Bestands-DTA Badenia: OZP-Objekte
      *
      *   Rev 2.0   Aug 14 2009 11:28:24   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2337                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2337                    *
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
      *    FÜR DIE FUNKTION DRKI2337
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2337.
           05  PI-DRKI2337-KEY.


             10 PI-DRKI2337-BAJGUTHPRODLFDNR   PIC S9(4)  COMP.
             10 PI-DRKI2337-BAJJAHR   PIC S9(4)  COMP.
             10 PI-DRKI2337-BAJVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2337-DATEN.


             10 PI-DRKI2337-BAJSALDWHNFOEKTO   PIC S9(7)V9(2)  COMP-3.
             10 PI-DRKI2337-N-BJSLDWHNFKT   PIC S9(4) COMP.
             10 PI-DRKI2337-BAJSUMAVWLEINZHL   PIC S9(7)V9(2)  COMP-3.
             10 PI-DRKI2337-N-BJSMVWLNZHL   PIC S9(4) COMP.
             10 PI-DRKI2337-BAJSUMFOEREINZHL   PIC S9(7)V9(2)  COMP-3.
             10 PI-DRKI2337-N-BJSMFRNZHL   PIC S9(4) COMP.
             10 PI-DRKI2337-BAJSUMFOERVORJHR   PIC S9(7)V9(2)  COMP-3.
             10 PI-DRKI2337-N-BJSMFRVRJHR   PIC S9(4) COMP.
             10 PI-DRKI2337-BAJVLSUMJAHR   PIC S9(7)V9(2)  COMP-3.
             10 PI-DRKI2337-NI-BAJVLSUMJAHR   PIC S9(4) COMP.
             10 PI-DRKI2337-BAJWOPAVORJHR   PIC S9(4)  COMP.
             10 PI-DRKI2337-NI-BAJWOPAVORJHR   PIC S9(4) COMP.
             10 PI-DRKI2337-BAJWOPAVORVORJHR   PIC S9(4)  COMP.
             10 PI-DRKI2337-N-BJWPVRVRJHR   PIC S9(4) COMP.
             

      *
       01  PO-DRKI2337.
           05  PO-DRKI2337-RC                 PIC  X(003).
               88 PO-DRKI2337-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2337-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
