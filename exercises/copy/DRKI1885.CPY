      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1885.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1885.CPv  $
      *
      *   Rev 2.4   Mar 04 2010 17:22:04   t13ex48
      *AKI00920: Teilbeträge für Hauptversicherung: Cobol-Sourcen
      *
      *   Rev 2.0   Apr 19 2006 11:18:14   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1885                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1885                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  26.02.2010/10:18:57
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
010310*AKI00920 !01.03.10!0002! Neue und geaenderte Feld- ! HSE      *
      *         !        !    ! namen in LEBENTEILSCHICHT !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1885
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1885.
           05  PI-DRKI1885-KEY.


             10 PI-DRKI1885-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI1885-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1885-LTSVERTRAGSID   PIC X(13).
             10 PI-DRKI1885-LTSZAHLDYNERHOEH   PIC S9(4) COMP.
             
      *
       01  PO-DRKI1885.
           05  PO-DRKI1885-RC                 PIC  X(003).
               88 PO-DRKI1885-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1885-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1885-DATEN.


             10 PO-DRKI1885-LTSSCHICHTBEITR   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI1885-LTSVERSSUMME   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI1885-NI-LTSVERSSUMME   PIC S9(4) COMP.
             10 PO-DRKI1885-LTSWKZ   PIC X(1).
             

      *
      ******* ENDE COPY-MEMBER **************************************
