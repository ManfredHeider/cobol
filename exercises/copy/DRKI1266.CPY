      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1266.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1266.CPv  $
      *
      *   Rev 2.4   Jun 26 2006 16:12:52   t13ex39
      *FKI01035: ZTKI0357 Fehler in DRKI1291
      *
      *   Rev 2.1   Jul 14 2003 10:05:20   t13ex39
      *AKI00327.UE005 DB-PRDUKTE
      *
      *   Rev 2.0   05 Jul 2001 17:07:04   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1266                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1266                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.46.47
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1266
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1266.
           05  PI-DRKI1266-KEY.


             10 PI-DRKI1266-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI1266-DATEN.


             10 PI-DRKI1266-VERABSCHLUSSGRUND.
                15 PI-DRKI1266-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI1266-VRBSCHLSSGRND-TXT   PIC X(30).

             10 PI-DRKI1266-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI1266-N-VRBSCHLSSVBNR   PIC S9(4) COMP.
             10 PI-DRKI1266-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI1266-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI1266-VEREIGENVERMKZ   PIC X(01).
             10 PI-DRKI1266-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1266-NI-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1266-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI1266-VERFREISTAUFTRAG   PIC X(01).
             10 PI-DRKI1266-VERFREMDVERTRAGKZ   PIC X(01).
             10 PI-DRKI1266-VERGEBUENDELTKZ   PIC X(01).
             10 PI-DRKI1266-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1266-N-VRGSMTBTRG   PIC S9(4) COMP.
             10 PI-DRKI1266-VERGESBEITRWKZ   PIC X(01).
             10 PI-DRKI1266-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI1266-N-VRNTRNVRTRNR   PIC S9(4) COMP.
             10 PI-DRKI1266-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI1266-N-VRMNTDRHPTF   PIC S9(4) COMP.
             10 PI-DRKI1266-VERNACHLASSKZ   PIC X(01).
             10 PI-DRKI1266-VERNOTIZID   PIC X(13).
             10 PI-DRKI1266-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1266-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI1266-NI-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI1266-VERREMDVERTRGESNAM.
                15 PI-DRKI1266-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI1266-VRRMDVRTRGSNM-TXT   PIC X(25).

             10 PI-DRKI1266-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI1266-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI1266-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI1266-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI1266-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI1266-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI1266-N-VRVRTRZSTNDD   PIC S9(4) COMP.
             10 PI-DRKI1266-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI1266-N-VRZHLNGSRTD   PIC S9(4) COMP.
             10 PI-DRKI1266-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PI-DRKI1266-N-VRZHLNGSWSD   PIC S9(4) COMP.


      *
       01  PO-DRKI1266.
           05  PO-DRKI1266-RC                 PIC  X(003).
               88 PO-DRKI1266-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1266-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
