      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0735.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0735.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Feb 1999 11:16:22   T13EX47
      *PARAMETER MIT GENOZP NEU ERSTELLT
      *
      *   Rev 1.1   29 Oct 1998 17:49:08   T13EX19
      *fki00010 - Neuerstellung
      *
      *   Rev 1.0   06 Oct 1998 15:34:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0735                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0735                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  11.02.99/16.22.01
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
      *    FÜR DIE FUNKTION DRKI0735
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0735.
           05  PI-DRKI0735-KEY.


             10 PI-DRKI0735-ADRESSEID   PIC X(13).

      *
       01  PO-DRKI0735.
           05  PO-DRKI0735-RC                 PIC  X(003).
               88 PO-DRKI0735-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0735-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************

      * ausgetauscht 11.02.99 gbe

      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0735 Funktion DELETE eine Zeile in PERSON *
      *    -------------------------------------------------------    *
      *    ERSTELLT: gbe                                             *
      *    DATUM:    16.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!15.10.98! 01 ! NEUERSTELLUNG             ! gbe      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0735                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PERSONENROLLE 'PERO'               *
      ****************************************************************
      *
      *01 PI-DRKI0735.
      *   05 PI-DRKI0735-ADRESSKEY.
      *      10 PI-DRKI0735-ADRESSEID          PIC X(13).
      *
      *01 PO-DRKI0735.
      *   05 PO-DRKI0735-RC                    PIC  X(003).
      *      88 PO-DRKI0735-RC-OK                         VALUE 'OK '.
      *      88 PO-DRKI0735-RC-NFD                        VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
