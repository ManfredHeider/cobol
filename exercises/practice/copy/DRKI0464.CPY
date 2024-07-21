      * I13PC18   Stand: 27/08/98    13:06:50
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0464.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0464.CPv  $
      *
      *   Rev 2.2   Jul 13 2007 10:16:58   t13ex51
      *AKI00664: Allfinanz: Bestandsdaten, Aktualisierung Vertrag
      *
      *   Rev 2.1   Jul 09 2007 18:29:02   t13ex51
      *AKI00664: Allfinanz: Bestandsdaten, Aktualisierung Vertrag
      *
      *   Rev 2.0   Feb 27 1999 12:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 07:49:00   I13PC48
      *Funktion Update fuer ZTKI0357
      *
      *   Rev 1.0   28 Jul 1998 09:56:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Gehört zu 463

      *****************************************************************
      *    COPY-MEMBER: DRKI0464                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0464                    *
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
      *    FÜR DIE FUNKTION DRKI0464
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0464.
           05  PI-DRKI0464-KEY.


             10 PI-DRKI0464-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0464-DATEN.


             10 PI-DRKI0464-VERABSCHLUSSGRUND.
                15 PI-DRKI0464-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI0464-VRBSCHLSSGRND-TXT   PIC X(30).

             10 PI-DRKI0464-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI0464-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI0464-VEREIGENVERMKZ   PIC X(01).
             10 PI-DRKI0464-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI0464-VERFREMDVERTRAGKZ   PIC X(01).
             10 PI-DRKI0464-VERGEBUENDELTKZ   PIC X(01).
             10 PI-DRKI0464-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0464-VERGESBEITRWKZ   PIC X(01).
             10 PI-DRKI0464-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI0464-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI0464-VERNACHLASSKZ   PIC X(01).
             10 PI-DRKI0464-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI0464-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI0464-VERREMDVERTRGESNAM.
                15 PI-DRKI0464-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI0464-VRRMDVRTRGSNM-TXT   PIC X(25).

             10 PI-DRKI0464-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI0464-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI0464-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI0464-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI0464-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI0464-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0464-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI0464-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
13ex51       10 PI-DRKI0464-VERMANDANTNR PIC S9(4) COMP.

      *
       01  PO-DRKI0464.
           05  PO-DRKI0464-RC                 PIC  X(003).
               88 PO-DRKI0464-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0464-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
