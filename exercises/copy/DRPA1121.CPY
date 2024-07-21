      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1121.CPv  $
      *
      *   Rev 2.1   Jan 21 2000 13:55:10   T13EX44
      *Datumsangaben erweitert
      *
      *   Rev 2.0   Mar 18 1999 08:11:32   T13EX44
      *Umstellung MFE
      *
      *   Rev 1.0   21 Oct 1998 13:58:12   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1121  RESTART PMPA0031                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    10.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.11.98! 01 ! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      * APA00073!03.01.00!2.01! ALLE JAHRESANGABEN AUF    ! SCHEIDEL *
      *         !        !    ! VIER STELLEN ERWEITERT    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
          01 RST-PMPA0031.
           05 RST-COPYNAME             PIC  X(008)  VALUE 'DRPA1121'.
           05 RST-PMPA0031-INFOS.
            10 RST-VNR                 PIC  9(015).
      *     10 RST-FAELLIG-VON         PIC  9(006).
030100      10 RST-FAELLIG-VON         PIC  9(008).
      *     10 RST-FAELLIG-BIS         PIC  9(006).
030100      10 RST-FAELLIG-BIS         PIC  9(008).
      *     10 RST-FAELLIG-BIS-ABR     PIC  9(006).
030100      10 RST-FAELLIG-BIS-ABR     PIC  9(008).
            10 RST-COMMIT-NACH         PIC S9(009)       BINARY.
            10 RST-SPEICHERORT         PIC  9(006).
