      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0190.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0190.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:32   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:22:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0190  FUNKTIONEN STANDARD-OZP            *
      *                           TBAT0021 NTZZ                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    08.11.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.11.97! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !06.02.98!01.02! NEUE FUNKTION DRAT0228   ! BOHLINGER*
      * APA00012!04.11.98!01.03! ZTATnnnn -> ZSATnnnn     ! MUND     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZSAT0021.
           03 PI-ZSAT0021-FUNKTION            PIC X(008).
              88 PI-ZSAT0021-DRAT0191-INS         VALUE 'DRAT0191'.
              88 PI-ZSAT0021-DRAT0192-UPD         VALUE 'DRAT0192'.
              88 PI-ZSAT0021-DRAT0193-DEL         VALUE 'DRAT0193'.
              88 PI-ZSAT0021-DRAT0194-SEL-ALL     VALUE 'DRAT0194'.
              88 PI-ZSAT0021-DRAT0202-DEL-ALL     VALUE 'DRAT0202'.
060298        88 PI-ZSAT0021-DRAT0228-COM         VALUE 'DRAT0228'.
