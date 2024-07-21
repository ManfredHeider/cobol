      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0237.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0237.cpv  $
      *
      *   Rev 2.1   Apr 29 1999 10:31:18   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:54:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:38   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:30   P13SP99
      *Initial revision.
      *
      *****************************************************************
      *    COPY-MEMBER: DRKI0237                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0237                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.04.99/11.51.21
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * fki00029!220499  ! 2  !                           ! vzg      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0237
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0237.
           05  PI-DRKI0237-KEY.


             10 PI-DRKI0237-BENMITAROLLENID   PIC X(13).
             10 PI-DRKI0237-BENMITAROLLENKZ   PIC X(01).

      *
       01  PO-DRKI0237.
           05  PO-DRKI0237-RC                 PIC  X(003).
               88 PO-DRKI0237-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0237-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
