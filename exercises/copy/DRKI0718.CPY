      * I13PC18   Stand: 20/10/98    10:23:40
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0718.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0718.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 18:01:58   T13EX48
      *AKI00028
      *SCHNITTSTELLE FÜR DIE 
      *FUNKTION DRKI0718
      *
      *   Rev 1.0   06 Oct 1998 15:32:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*


      *  Insert Doku-Eintrag

      *****************************************************************
      *    COPY-MEMBER: DRKI0718                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0718                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.10.98/10.19.08
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
      *    FÜR DIE FUNKTION DRKI0109
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0718.
           05  PI-DRKI0718-KEY.


             10 PI-DRKI0718-DOKUMENTID   PIC X(13).


           05  PI-DRKI0718-DATEN.


             10 PI-DRKI0718-ERSTELLDATUM   PIC X(10).
             10 PI-DRKI0718-ERSTELLERID   PIC X(13).
             10 PI-DRKI0718-ERSTELLERROLLE   PIC X(01).
             10 PI-DRKI0718-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI0718-SUBTYPEDOKUMENT   PIC X(4).

      *
       01  PO-DRKI0718.
           05  PO-DRKI0718-RC                 PIC  X(003).
               88 PO-DRKI0718-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0718-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
