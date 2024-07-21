      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0185.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0185.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:12   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0185  FUNKTIONEN STANDARD-OZP            *
      *                           TBAT0020 NTZ                       *
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
      * APA00012!08.11.97! 01  ! NEUERSTELLUNG            ! BOHLINGER*
      *         !06.02.98!01.02! NEUE FUNKTION DRAT0228   ! BOHLINGER*
      * APA00012!04.11.98!01.03! ZTATnnnn -> ZSATnnnn     ! MUND     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZSAT0020.
           03 PI-ZSAT0020-FUNKTION            PIC X(008).
              88 PI-ZSAT0020-DRAT0186-INS         VALUE 'DRAT0186'.
              88 PI-ZSAT0020-DRAT0187-UPD         VALUE 'DRAT0187'.
              88 PI-ZSAT0020-DRAT0188-DEL         VALUE 'DRAT0188'.
              88 PI-ZSAT0020-DRAT0189-SEL-ALL     VALUE 'DRAT0189'.
060298        88 PI-ZSAT0020-DRAT0228-COM         VALUE 'DRAT0228'.
