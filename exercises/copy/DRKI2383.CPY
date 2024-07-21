      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2383.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2383.CPv  $
      *
      *   Rev 2.1   Apr 19 2010 15:25:52   t13ex14
      *AKI00935: Hauptprogramm und Upro
      *
      *   Rev 2.0   Mar 04 2010 10:52:04   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2375                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2375                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00935!25.02.10! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2383
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT LEBENPERSONEIGEN <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2383.
           05  PI-DRKI2383-KEY.
             10 PI-DRKI2383-GES           PIC  9(002).
             10 PI-DRKI2383-VNR           PIC  9(015).

           05  PI-DRKI2383-DATEN.
             10 PI-DRKI2383-ROLLE-VERTRAG PIC  X(001).
             10 PI-DRKI2383-ROLLE-TARIF   PIC  X(001).
             10 PI-DRKI2383-VTRBEZ-ARTKZ  PIC  X(001).


       01  PO-DRKI2383.

           05  PO-DRKI2383-DATEN.
             10 PO-DRKI2383-ANREDE        PIC S9(002).
             10 PO-DRKI2383-NNAME         PIC  X(030).
             10 PO-DRKI2383-VNAME         PIC  X(030).
             10 PO-DRKI2383-PLZ           PIC  X(015).
             10 PO-DRKI2383-ORT           PIC  X(040).
             10 PO-DRKI2383-STR           PIC  X(045).
             10 PO-DRKI2383-AUSL-KZ       PIC  X(003).
             10 PO-DRKI2383-GEBDAT        PIC  X(010).
      *
           05  PO-DRKI2383-RC             PIC  X(003).

               88 PO-DRKI2383-RC-OK              VALUE 'OK '.
               88 PO-DRKI2383-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2383-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2383-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2383-RC-INSERT          VALUE 'INS'.
               88 PO-DRKI2383-RC-TU-NICHTS       VALUE 'NOP'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2383-FEHLERSATZ.
               10 PO-DRKI2383-FEHLER-FUNKTION    PIC X(10).
               10 PO-DRKI2383-FEHLER-TABELLE     PIC X(20).
               10 PO-DRKI2383-FEHLER-SQL-FEHLER  PIC X(04).
               10 PO-DRKI2383-FEHLER-KEY         PIC X(40).
      ******* ENDE COPY-MEMBER **************************************
