      * P13SP99   Stand: 06/12/98    18:02:09
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0599.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0599.CPv  $
      *
      *   Rev 2.2   Jan 21 2003 16:26:08   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.1   Apr 22 1999 17:04:54   T13EX48
      *AKI00028
      *RDTA
      *
      *   Rev 2.0   Apr 22 1999 16:49:08   T13EX48
      * 
      *
      *   Rev 1.0   07 Aug 1998 14:51:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0599  DATEIAUFBAU RUECKDTA AMLEBEN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    18.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * AKI00303!07.01.03!2.2 ! Erweiterung für Kommisa-  ! IFR      *
      *         !        !    ! rische Kundenübertragung  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0579                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
      *
       01  DRKI0599-SATZ.
           10 DRKI0599-AENDERUNGSKZ           PIC 9(002).
           10 DRKI0599-VERTRAGSNR             PIC 9(015).
           10 DRKI0599-DIREKTIONSNR           PIC 9(003).
           10 DRKI0599-KUNDENBETREUERNR       PIC 9(007).
070103     10 DRKI0599-KZ-KOMM-BETR           PIC X(001).
