      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0267.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0267.CPv  $
      *
      *   Rev 2.1   Jul 03 2020 07:07:48   t13ex67
      *FPA01775: Abend beim Aufruf einer EVS-Notiz 
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:58:12   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 08:38:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0268 FUNKTION ERMITTELN EINER NOTIZ      *
      *                                   ZU EINER ART VON OBJEKT    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                           *
      *    DATUM:    23.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.06.98! 01 !NEU                        !FOLTZ     *
      * APA00012!03.09.98!1.02! ALLGEMEINGüLTIGE SCHNITT- !BOHLINGER *
      *         !        !    ! STELLE FüR ALLE DRUCKAUF- !          *
      *         !        !    ! BEREITUNGSPROGRAMME       !          *
      * FPA01775!01.07.20!2.01! NEUE FUNKTION DRAT0626    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRUCK-PGM.
           03 PI-DRUCK-PGM-NAME              PIC X(008).
           03 PI-DRUCK-PGM-FUNKTION          PIC X(008).
              88 PI-DRUCK-PGM-DRAT0268           VALUE 'DRAT0268'.
              88 PI-DRUCK-PGM-DRAT0626           VALUE 'DRAT0626'.
      ****************************************************************



