      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0236.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0236.cpv  $
      *
      *   Rev 2.1   Apr 29 1999 10:30:32   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:54:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:36   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0236                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0236                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.04.99/11.51.21
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *fki00029 '220499  !2   !                           !vzg       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0236
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0236.
           05  PI-DRKI0236-KEY.


             10 PI-DRKI0236-BENMITAROLLENID   PIC X(13).
             10 PI-DRKI0236-BENMITAROLLENKZ   PIC X(01).


           05  PI-DRKI0236-DATEN.


             10 PI-DRKI0236-BENKENNWORT   PIC X(15).

      *
       01  PO-DRKI0236.
           05  PO-DRKI0236-RC                 PIC  X(003).
               88 PO-DRKI0236-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0236-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
