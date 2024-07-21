      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1291.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI1291.CPv  $
      *
      *   Rev 2.8   Nov 09 2007 11:41:00   t13ex14
      *AKL00664: Kfz- u. REOB PK-Ermittlung u. Aktualisierung
      *
      *   Rev 2.6   Jun 26 2006 16:11:14   t13ex39
      *FKI01035: ZTKI0357 Fehler in DRKI1291
      *
      *   Rev 2.2   Nov 03 2004 15:00:42   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank
      *
      *   Rev 2.0   05 Jul 2001 17:07:28   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1291                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1291                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.10.2007/10:42:45
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
      *    FÜR DIE FUNKTION DRKI1291
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1291.
           05  PI-DRKI1291-KEY.


             10 PI-DRKI1291-VERVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1291.
           05  PO-DRKI1291-RC                 PIC  X(003).
               88 PO-DRKI1291-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1291-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1291-DATEN.


             10 PO-DRKI1291-VERABSCHLUSSGRUND.
                15 PO-DRKI1291-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PO-DRKI1291-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PO-DRKI1291-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI1291-VERAENDERUNGSDATUM   PIC X(26).
             10 PO-DRKI1291-VERDATPRODLOESCH   PIC X(10).
             10 PO-DRKI1291-VERDATVERTRAEND   PIC X(10).
             10 PO-DRKI1291-VEREIGENVERMKZ   PIC X(1).
             10 PO-DRKI1291-VERFRALFDNR   PIC S9(4) COMP.
             10 PO-DRKI1291-VERFRAPERSONENID   PIC X(13).
             10 PO-DRKI1291-VERFREISTAUFTRAG   PIC X(1).
             10 PO-DRKI1291-VERFREMDVERTRAGKZ   PIC X(1).
             10 PO-DRKI1291-VERGEBUENDELTKZ   PIC X(1).
             10 PO-DRKI1291-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1291-VERGESBEITRWKZ   PIC X(1).
             10 PO-DRKI1291-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PO-DRKI1291-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PO-DRKI1291-VERNACHLASSKZ   PIC X(1).
             10 PO-DRKI1291-VERNOTIZID   PIC X(13).
             10 PO-DRKI1291-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PO-DRKI1291-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PO-DRKI1291-VERREMDVERTRGESNAM.
                15 PO-DRKI1291-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PO-DRKI1291-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PO-DRKI1291-VERSTORNODATUM   PIC X(10).
             10 PO-DRKI1291-VERVERTRAGSABLAUF   PIC X(10).
             10 PO-DRKI1291-VERVERTRAGSBEGINN   PIC X(10).
             10 PO-DRKI1291-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI1291-VERVERTRZUSTANDDAT   PIC X(10).
             10 PO-DRKI1291-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PO-DRKI1291-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PO-DRKI1291-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
