      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0151.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0151.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 Aug 1997 17:46:38   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.0   18 Aug 1997 10:26:50   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0151                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0151                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Opritz                                           *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0022!18.08.97!    ! NEU                       ! Opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0151: LESEN TBAT0008  MIT:
      *                               KTY-NR
      *                               KNR-NR
      *
      ****************************************************************
      *
       01  PI-DRAT0151.
           05  PI-DRAT0151-DATEN.
               10 PI-DRAT0151-KTY-NR           PIC S9(004) COMP.
               10 PI-DRAT0151-KNR-NR           PIC S9(004) COMP.
      *
       01  PO-DRAT0151.
           05  PO-DRAT0151-RC                  PIC  X(003).
               88 PO-DRAT0151-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0151-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0151-DATEN.
               10 PO-DRAT0151-KNR-LAST-UPDATE  PIC  X(026).
               10 PO-DRAT0151-KNR-BEZ          PIC  X(080).
               10 PO-DRAT0151-KNR-BEZ-KURZ     PIC  X(004).
               10 PO-DRAT0151-KNR-BEZ10        PIC  X(010).
               10 PO-DRAT0151-KNR-HINDERNIS    PIC  X(001).
      *
