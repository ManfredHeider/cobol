      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0951.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0951.CPv  $
      *
      *   Rev 2.1   Apr 14 2000 08:45:54   T13EX49
      *AKI00071 - erstmalige Übergabe
      *
      *   Rev 2.0   Nov 29 1999 13:55:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0951                                      *
      *    --------------------------------------------------------   *
      *    VB-Schnittstelle - Entladedatei für Logtabelle VIAT0005    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *    DATUM:    07. 01. 2000                                     *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00071!07.01.00! 01 ! NEU                       !Peter Pohl*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *---------------------------------------------------------------*
       01  EIN-SATZ1.
           05 LOG-MA-NR                    PIC 9(07)  COMP.
           05 LOG-TIMESTAMP                PIC X(26).
           05 LOG-TABLE-ID                 PIC X(08).
           05 KNR-ST-TYP                   PIC S9(004)  BINARY.

