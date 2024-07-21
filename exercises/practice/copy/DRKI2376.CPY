      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2376.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2376.CPv  $
      *
      *   Rev 2.6   Feb 12 2015 10:24:04   t13ex27
      *AKI01322: SEPA - Restarbeiten-UPKI0218
      *
      *   Rev 2.5   Sep 20 2012 09:55:42   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.4   Sep 06 2012 14:32:24   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 11:21:02   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:08:22   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Apr 19 2010 15:25:40   t13ex14
      *AKI00935: Hauptprogramm und Upro
      *
      *   Rev 2.0   Feb 23 2010 14:29:34   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2376                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2376                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00935!25.02.10! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
210611* AKI00886!21.06.11!    ! Aufrufschnittstelle geänd.! WMI      *
      * AKI01322!19.08.24!2.6 ! SEPA-Nacharbeiten         ! IFR      *
      *         !        !    ! Entfernen Beitragszahler  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2376
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE LEBENPERSONEIGEN <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2376.

           05  PI-DRKI2376-KEY.

             10 PI-DRKI2376-GES           PIC  9(002).
             10 PI-DRKI2376-VNR           PIC  9(015).
210611       10 PI-DRKI2376-VERTRAGSID    PIC  X(013).
210611       10 PI-DRKI2376-ADRESSEID     PIC  X(013).

           05  PI-DRKI2376-DATEN.
210611       10 PI-DRKI2376-PERSROLLEKZ   PIC  X(001).
210611*      10 PI-DRKI2376-ROLLE-VERTRAG PIC  X(001).
210611*      10 PI-DRKI2376-ROLLE-TARIF   PIC  X(001).
210611*      10 PI-DRKI2376-VTRBEZ-ARTKZ  PIC  X(001).
210611*         ROLLE-VERTRAG = SPACE und ROLLE-TARIF = 'P' und
210611*           VTRBEZ-ARTKZ = SPACE
210611*             'H'auptversichete Person
210611*           VTRBEZ-ARTKZ = 'M'
210611*             'M'itversicherte Person
210611*           VTRBEZ-ARTKZ = '2'
210611*             '2'. mitversicherte Person

             10 PI-DRKI2376-ANREDE        PIC S9(002).
             10 PI-DRKI2376-NNAME         PIC  X(030).
             10 PI-DRKI2376-VNAME         PIC  X(030).
210611*      10 PI-DRKI2376-PLZ           PIC  X(015).
210611*      10 PI-DRKI2376-ORT           PIC  X(040).
210611*      10 PI-DRKI2376-STR           PIC  X(045).
210611*      10 PI-DRKI2376-AUSL-KZ       PIC  X(003).
             10 PI-DRKI2376-GEBDAT        PIC  X(010).
             10 PI-DRKI2376-AKTUALKZ      PIC  X(001).
      *        U = UPDATE


      *
       01  PO-DRKI2376.

           05  PO-DRKI2376-RC             PIC  X(003).
               88 PO-DRKI2376-RC-OK              VALUE 'OK '.
               88 PO-DRKI2376-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2376-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2376-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2376-RC-INSERT          VALUE 'INS'.
               88 PO-DRKI2376-RC-TU-NICHTS       VALUE 'NOP'.

           05  PO-DRKI2376-ROLLE          PIC  X(001).
               88 PO-DRKI2376-ROLLE-H            VALUE 'H'.
               88 PO-DRKI2376-ROLLE-M            VALUE 'M'.
               88 PO-DRKI2376-ROLLE-2M           VALUE '2'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2376-FEHLERSATZ.
               10 PO-DRKI2376-FEHLER-FUNKTION    PIC  X(10).
               10 PO-DRKI2376-FEHLER-TABELLE     PIC  X(20).
               10 PO-DRKI2376-FEHLER-SQL-FEHLER  PIC S9(04).
               10 PO-DRKI2376-FEHLER-KEY         PIC  X(40).
      ******* ENDE COPY-MEMBER ***************************************

