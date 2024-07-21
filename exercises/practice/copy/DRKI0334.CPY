      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0334.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0334.cpv  $
      *
      *   Rev 2.1   Apr 29 1999 10:31:42   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:57:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:49:08   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   14 Jul 1998 17:00:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0334                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0334                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.04.99/11.51.21
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * fki00029!220499  !2   !                           !  vzg     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0334
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0334.
           05  PI-DRKI0334-KEY.


             10 PI-DRKI0334-BENMITAROLLENID   PIC X(13).
             10 PI-DRKI0334-BENMITAROLLENKZ   PIC X(01).

      *
       01  PO-DRKI0334.
           05  PO-DRKI0334-RC                 PIC  X(003).
               88 PO-DRKI0334-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0334-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0334-DATEN.


             10 PO-DRKI0334-BENKENNWORT   PIC X(15).


      *
      ******* ENDE COPY-MEMBER **************************************
