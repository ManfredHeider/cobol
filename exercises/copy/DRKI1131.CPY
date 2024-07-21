      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1131.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1131.CPv  $
      *
      *   Rev 2.2   Jan 11 2017 10:08:30   t13ae31
      *AKI01475: Partnertexte angleichen DB2
      *
      *   Rev 2.1   14 Nov 2000 10:10:02   T13EX39
      *AKI00068 KBI
      *
      *   Rev 1.0   14 Jul 1998 17:02:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1131                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1131                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.09.00/15.17.38
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
020117* AKI01475!02.01.17!    ! PARTGESELLSCHNAME 25 in 50! P. Hübner*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1131
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1131.
           05  PI-DRKI1131-KEY.
 
 
             10 PI-DRKI1131-PARTGESELLSCHAFTID   PIC S9(4) COMP.
 
      *
       01  PO-DRKI1131.
           05  PO-DRKI1131-RC                 PIC  X(003).
               88 PO-DRKI1131-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1131-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1131-DATEN.
 
 
             10 PO-DRKI1131-PARTGESELLSCHNAME   PIC X(60).
 
 
      *
      ******* ENDE COPY-MEMBER **************************************

