      * I13PC18   Stand: 20/10/98    10:23:48
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0728.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0728.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 18:06:16   T13EX48
      *AKI00028
      *PI/PO FUNKTION DRKI0728 
      *
      *   Rev 1.0   06 Oct 1998 15:33:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*

      *  Insert Gesamte Tabelle

      *****************************************************************
      *    COPY-MEMBER: DRKI0728                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0728                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.10.98/16.10.49
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
      *    FÜR DIE FUNKTION DRKI0728
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0728.
           05  PI-DRKI0728-KEY.


             10 PI-DRKI0728-RUECKLAUFKONTRID   PIC X(13).


           05  PI-DRKI0728-DATEN.


             10 PI-DRKI0728-ABSENDEDATUM   PIC X(10).
             10 PI-DRKI0728-BESCHREIBUNG   PIC X(01).
             10 PI-DRKI0728-BESUCHSAUFTRART   PIC X(01).
             10 PI-DRKI0728-ERLEDIGTAMDATUM   PIC X(10).
             10 PI-DRKI0728-ERLEDIGUNGSKZ   PIC S9(4) COMP.
             10 PI-DRKI0728-GESELLSCHAFT   PIC S9(4) COMP.
             10 PI-DRKI0728-GESELLSCHAFTSKZ   PIC S9(4) COMP.
             10 PI-DRKI0728-KUNDENNAME   PIC X(30).
             10 PI-DRKI0728-KUNDENNUMMER   PIC S9(9) COMP.
             10 PI-DRKI0728-PCVBPC   PIC S9(9) COMP.
             10 PI-DRKI0728-PCVWPC   PIC S9(9) COMP.
             10 PI-DRKI0728-VBNUMMER   PIC S9(9) COMP.
             10 PI-DRKI0728-VERSICHERUNGSNR   PIC X(15).

      *
       01  PO-DRKI0728.
           05  PO-DRKI0728-RC                 PIC  X(003).
               88 PO-DRKI0728-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0728-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
