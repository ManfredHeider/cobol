      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0136.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0136.CPv  $
      *
      *   Rev 2.1   30 Oct 2000 12:27:06   T13AE27
      *Dim7 für AVBG
      *
      *   Rev 2.0   Feb 27 1999 15:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 21 1997 10:05:46   T13EX11
      *AFVB0331: Satzbeschreibung für OLZ-Jahreswerte
      *          für VB-Abrechnung
      *
      *   Rev 1.0   Feb 19 1997 09:57:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: SATZBESCHREIBUNG OLZ-JAHRESWERTE              *
      *                 FÜR VB-ABRECHNUNG                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER                                  *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0331!10.02.97! 01 ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      * ASL00060!04.09.00! 02 ! Anpassungen wegen AVBG    ! P.Dürr   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRVB0136.
           05  DRVB0136-VB-NR-10.
               10  DRVB0136-VB-NR-7      PIC  9(007).
               10  DRVB0136-VB-NR-FILLER PIC  x(002).
               10  DRVB0136-VB-NR-P      PIC  X(001).
           05  DRVB0136-BETRAG-KUM       PIC S9(009)V99 PACKED-DECIMAL.
040900*    05  FILLER                    PIC  X(006).
040900     05  DRVB0136-WAEHR            PIC S9(004) COMP.
040900     05  FILLER                    PIC  X(004).
      *
      *    VORSATZ: 1. SATZ DER DATEI
       01  DRVB0136-VORSATZ            REDEFINES DRVB0136.
           05  DRVB0136-V-VB-NR-10       PIC  X(010).
           05  DRVB0136-V-BUCHDAT-ISO    PIC  X(010).
           05  DRVB0136-V-MONAT-BIS      PIC  9(002).
      *
      ******** ENDE COPY-MEMBER (DRVB0136) ************

