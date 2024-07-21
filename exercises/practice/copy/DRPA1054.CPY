      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1054.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1054.CPv  $
      *
      *   Rev 2.1   May 27 1999 12:35:12   T13EX26
      *NT-Umstellung
      *
      *   Rev 2.0   May 26 1999 16:58:20   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 14:01:14   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1054  FUNKTION DRPA1054 LIST   PDPT/PDPE *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    25.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!25.08.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      * APA00012!29.01.99! 02 ! UMSTELLUNG AUF LESEN MIT  ! PARENT   *
      *         !        !    ! LESE-OZP STATT LIST       !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1054.
290199*   05 PI-DRPA1054-UFKT-CODE                  PIC  9(002).
          05 PI-DRPA1054-DATEN.
             10 PI-DRPA1054-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
290199*      10 PI-DRPA1054-PMPT-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1054.
          05 PO-DRPA1054-RC                         PIC  X(003).
             88 PO-DRPA1054-RC-OK                         VALUE 'OK '.
290199       88 PO-DRPA1054-RC-NOT-FOUND                  VALUE 'NFD'.
290199*      88 PO-DRPA1054-RC-LEER                       VALUE 'LER'.
290199*      88 PO-DRPA1054-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1054-PDPT-PDPE.
      *
          COPY DRPA0451.
      *
      ******* ENDE COPY-MEMBER **************************************
