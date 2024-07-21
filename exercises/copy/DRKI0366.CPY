      * I13PC18   Stand: 25/08/98    09:26:32
      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0366.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\drki0366.cpv  $
      *
      *   Rev 2.3   Sep 03 1999 12:55:58   T13EX39
      *AKI00044 WKZ eingebaut
      *
      *   Rev 2.2   Apr 15 1999 11:29:36   T13EX50
      *keine ?nderung
      *
      *   Rev 2.1   Mar 23 1999 15:46:20   T13EX33
      *aki00001 keine ?nderung
      *
      *   Rev 2.0   Feb 27 1999 12:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:06:02   I13PC18
      *FUNKTION UPDATE FUER VERTRAG
      *
      *   Rev 1.0   14 Jul 1998 17:02:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Update einige (AML)

      *****************************************************************
      *    COPY-MEMBER: DRKI0366                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0366                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.08.99/12.26.48
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
      *    FÜR DIE FUNKTION DRKI0366
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0366.
           05  PI-DRKI0366-KEY.


             10 PI-DRKI0366-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0366-DATEN.


             10 PI-DRKI0366-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI0366-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI0366-VERGESBEITRWKZ   PIC X(01).
             10 PI-DRKI0366-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI0366-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI0366-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI0366-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI0366-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0366-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI0366-VERZAHLUNGSWEISEID   PIC S9(4) COMP.


      *
       01  PO-DRKI0366.
           05  PO-DRKI0366-RC                 PIC  X(003).
               88 PO-DRKI0366-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0366-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
