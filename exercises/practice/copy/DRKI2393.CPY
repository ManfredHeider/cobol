      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2393.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2393.CPv  $
      *
      *   Rev 2.4   Sep 20 2012 09:56:06   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 14:32:48   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:21:38   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Sep 06 2012 11:08:56   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.0   Mar 22 2010 12:40:54   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2393                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2393                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00943!22.03.10! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2393
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT KRANKENPERSEIGEN <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2393.

           05  PI-DRKI2393-KEY.

             10 PI-DRKI2393-VERTRAGSID     PIC  X(013).
             10 PI-DRKI2393-PERSONLFDNR    PIC  9(002).
             10 PI-DRKI2393-RUF-PGM        PIC  X(008).


           05  PI-DRKI2393-DATEN.

             10 PI-DRKI2393-GES            PIC  9(002).
             10 PI-DRKI2393-VNR            PIC  9(015).
             10 PI-DRKI2393-ADRESSEID      PIC  X(013).
             10 PI-DRKI2393-PERSONENID     PIC  X(013).
      *      10 PI-DRKI2393-ROLLE-VERTRAG  PIC  X(001).
      *      10 PI-DRKI2393-ROLLE-TARIF    PIC  X(001).
             10 PI-DRKI2393-PERSROLLEKZ    PIC  X(002).

             10 PI-DRKI2393-ANREDE         PIC S9(002).
             10 PI-DRKI2393-NNAME          PIC  X(030).
             10 PI-DRKI2393-VNAME          PIC  X(030).
             10 PI-DRKI2393-GEBDAT         PIC  X(010).
             10 PI-DRKI2393-BEGINNDATUM    PIC  X(010).
             10 PI-DRKI2393-ENDEDATUM      PIC  X(010).

      *
       01  PO-DRKI2393.

           05  PO-DRKI2393-RC             PIC  X(003).
               88 PO-DRKI2393-RC-OK              VALUE 'OK '.
               88 PO-DRKI2393-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2393-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2393-RC-INSERT          VALUE 'INS'.
               88 PO-DRKI2393-RC-TU-NICHTS       VALUE 'NOP'.

           05  PO-DRKI2393-ROLLE          PIC  X(002).
               88 PO-DRKI2393-ROLLE-NP           VALUE 'NP'.
               88 PO-DRKI2393-ROLLE-N            VALUE 'N '.
               88 PO-DRKI2393-ROLLE-P            VALUE ' P'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2393-FEHLERSATZ.
               10 PO-DRKI2393-FEHLER-FUNKTION    PIC  X(10).
               10 PO-DRKI2393-FEHLER-TABELLE     PIC  X(20).
               10 PO-DRKI2393-FEHLER-SQL-FEHLER  PIC S9(04).
               10 PO-DRKI2393-FEHLER-KEY         PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2393-FEHLER-KEY.
                  15 PO-DRKI2393-GES             PIC  9(002).
                  15 PO-DRKI2393-VNR             PIC  X(015).
      ******* ENDE COPY-MEMBER **************************************
