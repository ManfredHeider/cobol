      * I13PC18   Stand: 27/08/98    13:06:47
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0463.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0463.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 07:48:16   I13PC48
      *Funktion Update fuer ZTKI0357
      *
      *   Rev 1.0   28 Jul 1998 09:56:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0463                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0463                    *
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
      *    FÜR DIE FUNKTION DRKI0463
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0463.
           05  PI-DRKI0463-KEY.


             10 PI-DRKI0463-VERVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0463.
           05  PO-DRKI0463-RC                 PIC  X(003).
               88 PO-DRKI0463-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0463-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0463-DATEN.


             10 PO-DRKI0463-VERABSCHLUSSGRUND.
                15 PO-DRKI0463-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PO-DRKI0463-VRBSCHLSSGRND-TXT   PIC X(30).

             10 PO-DRKI0463-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI0463-VERAENDERUNGSDATUM   PIC X(26).
             10 PO-DRKI0463-VERDATVERTRAEND   PIC X(10).
             10 PO-DRKI0463-VEREIGENVERMKZ   PIC X(01).
             10 PO-DRKI0463-VERFRALFDNR   PIC S9(4) COMP.
             10 PO-DRKI0463-VERFRAPERSONENID   PIC X(13).
             10 PO-DRKI0463-VERFREISTAUFTRAG   PIC X(01).
             10 PO-DRKI0463-VERFREMDVERTRAGKZ   PIC X(01).
             10 PO-DRKI0463-VERGEBUENDELTKZ   PIC X(01).
             10 PO-DRKI0463-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0463-VERGESBEITRWKZ   PIC X(01).
             10 PO-DRKI0463-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PO-DRKI0463-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PO-DRKI0463-VERNACHLASSKZ   PIC X(01).
             10 PO-DRKI0463-VERNOTIZID   PIC X(13).
             10 PO-DRKI0463-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PO-DRKI0463-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PO-DRKI0463-VERREMDVERTRGESNAM.
                15 PO-DRKI0463-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PO-DRKI0463-VRRMDVRTRGSNM-TXT   PIC X(25).

             10 PO-DRKI0463-VERSTORNODATUM   PIC X(10).
             10 PO-DRKI0463-VERVERTRAGSABLAUF   PIC X(10).
             10 PO-DRKI0463-VERVERTRAGSBEGINN   PIC X(10).
             10 PO-DRKI0463-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI0463-VERVERTRZUSTANDDAT   PIC X(10).
             10 PO-DRKI0463-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PO-DRKI0463-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PO-DRKI0463-VERZAHLUNGSWEISEID   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
