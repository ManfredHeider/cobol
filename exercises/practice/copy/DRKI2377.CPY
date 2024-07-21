      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2377.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2377.CPv  $
      *
      *   Rev 2.6   Feb 12 2015 10:24:08   t13ex27
      *AKI01322: SEPA - Restarbeiten-UPKI0218
      *
      *   Rev 2.5   Sep 20 2012 09:55:48   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.4   Sep 06 2012 14:32:30   t13ae49
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.3   Sep 06 2012 11:21:12   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.2   Sep 06 2012 11:08:30   t13ex14
      *AKI00886: NKDA: Upros VN / BZ / VP insert, update, delete
      *
      *   Rev 2.1   Apr 19 2010 15:25:46   t13ex14
      *AKI00935: Hauptprogramm und Upro
      *
      *   Rev 2.0   Feb 23 2010 14:29:40   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2377                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2377                    *
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
      *    FÜR DIE FUNKTION DRKI2377
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2377.
           05  PI-DRKI2377-KEY.

             10 PI-DRKI2377-GES           PIC  9(002).
             10 PI-DRKI2377-VNR           PIC  9(015).
210611       10 PI-DRKI2377-VERTRAGSID    PIC  X(013).
210611       10 PI-DRKI2377-ADRESSEID     PIC  X(013).
             10 PI-DRKI2377-PERSROLLEKZ   PIC  X(001).
      *
           05  PI-DRKI2377-DATEN.

210611*      10 PI-DRKI2377-ROLLE-VERTRAG PIC  X(001).
210611*      10 PI-DRKI2377-ROLLE-TARIF   PIC  X(001).
210611*      10 PI-DRKI2377-VTRBEZ-ARTKZ  PIC  X(001).
210611*         ROLLE-VERTRAG = SPACE und ROLLE-TARIF = 'P' und
210611*           VTRBEZ-ARTKZ = SPACE
210611*             'H'auptversichete Person
210611*           VTRBEZ-ARTKZ = 'M'
210611*             'M'itversicherte Person
210611*           VTRBEZ-ARTKZ = '2'
210611*             '2'. mitversicherte Person
             10 PI-DRKI2377-AKTUALKZ      PIC  X(001).
      *        D = DELETE

       01  PO-DRKI2377.

           05  PO-DRKI2377-RC             PIC  X(003).
               88 PO-DRKI2377-RC-OK              VALUE 'OK '.
               88 PO-DRKI2377-RC-DUPLICATE-VALUE VALUE 'DVA'.
               88 PO-DRKI2377-RC-NOT-FOUND       VALUE 'NFD'.
               88 PO-DRKI2377-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2377-RC-INSERT          VALUE 'INS'.
               88 PO-DRKI2377-RC-TU-NICHTS       VALUE 'NOP'.

           05  PO-DRKI2377-ROLLE          PIC  X(001).
               88 PO-DRKI2377-ROLLE-H            VALUE 'H'.
               88 PO-DRKI2377-ROLLE-M            VALUE 'M'.
               88 PO-DRKI2377-ROLLE-2M           VALUE '2'.
      *
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2377-FEHLERSATZ.
               10 PO-DRKI2377-FEHLER-FUNKTION    PIC  X(10).
               10 PO-DRKI2377-FEHLER-TABELLE     PIC  X(20).
               10 PO-DRKI2377-FEHLER-SQL-FEHLER  PIC S9(04).
               10 PO-DRKI2377-FEHLER-KEY         PIC  X(40).
      ******* ENDE COPY-MEMBER **************************************

