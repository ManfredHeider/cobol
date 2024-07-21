      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.10.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1258.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1258.CPv  $
      *
      *   Rev 2.1   Oct 19 2005 11:58:40   t13ex27
      *AKI00509: Sonderzahlungen in die Wunschpolice - Zusatzanford
      *erung
      *
      *   Rev 2.0   Oct 04 2005 15:46:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1258                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1258                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  04.10.2005/14:12:23
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
      *    FÜR DIE FUNKTION DRKI1258
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1258.
           05  PI-DRKI1258-KEY.


             10 PI-DRKI1258-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI1258-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1258-LTSVERTRAGSID   PIC X(13).
             10 PI-DRKI1258-LTSZAHLDYNERHOEH   PIC S9(4) COMP.


           05  PI-DRKI1258-DATEN.


             10 PI-DRKI1258-LTSBEITRZUSTID   PIC X(4).
             10 PI-DRKI1258-LTSDATDYNERHOEH   PIC X(10).
             10 PI-DRKI1258-LTSRATENBEITRAG   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI1258-NI-LTSRATENBEITRAG   PIC S9(4) COMP.
             10 PI-DRKI1258-LTSRATENBEITRWKZ   PIC X(01).
             10 PI-DRKI1258-LTSTARBEZEICHNUNG   PIC X(15).
             10 PI-DRKI1258-LTSVERSSUMME   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI1258-NI-LTSVERSSUMME   PIC S9(4) COMP.
             10 PI-DRKI1258-LTSVERSSUMMEWKZ   PIC X(01).
             10 PI-DRKI1258-LTSVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI1258-LTSVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI1258-N-LTSVRTRZSTNDD   PIC S9(4) COMP.


      *
       01  PO-DRKI1258.
           05  PO-DRKI1258-RC                 PIC  X(003).
               88 PO-DRKI1258-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1258-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
