      * I13PC18   Stand: 27/08/98    13:06:54
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0465.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0465.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 07:49:28   I13PC48
      *Funktion fuer Update ZTKI0357
      *
      *   Rev 1.0   28 Jul 1998 09:56:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0465                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0465                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/12.59.21
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
      *    FÜR DIE FUNKTION DRKI0465
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0465.
           05  PI-DRKI0465-KEY.


             10 PI-DRKI0465-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0465-DATEN.


             10 PI-DRKI0465-VERABSCHLUSSGRUND.
                15 PI-DRKI0465-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI0465-VRBSCHLSSGRND-TXT   PIC X(30).

             10 PI-DRKI0465-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI0465-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI0465-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI0465-VEREIGENVERMKZ   PIC X(01).
             10 PI-DRKI0465-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI0465-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI0465-VERFREISTAUFTRAG   PIC X(01).
             10 PI-DRKI0465-VERFREMDVERTRAGKZ   PIC X(01).
             10 PI-DRKI0465-VERGEBUENDELTKZ   PIC X(01).
             10 PI-DRKI0465-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0465-VERGESBEITRWKZ   PIC X(01).
             10 PI-DRKI0465-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI0465-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI0465-VERNACHLASSKZ   PIC X(01).
             10 PI-DRKI0465-VERNOTIZID   PIC X(13).
             10 PI-DRKI0465-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI0465-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI0465-VERREMDVERTRGESNAM.
                15 PI-DRKI0465-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI0465-VRRMDVRTRGSNM-TXT   PIC X(25).

             10 PI-DRKI0465-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI0465-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI0465-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI0465-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI0465-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI0465-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0465-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI0465-VERZAHLUNGSWEISEID   PIC S9(4) COMP.


      *
       01  PO-DRKI0465.
           05  PO-DRKI0465-RC                 PIC  X(003).
               88 PO-DRKI0465-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0465-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
