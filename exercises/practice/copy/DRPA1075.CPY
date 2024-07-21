      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1075.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1075.CPv  $
      *
      *   Rev 2.0   Mar 18 1999 08:07:26   T13EX44
      *Umstellung MFE
      *
      *   Rev 1.0   20 Jul 1998 10:20:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1075  BESTANDS-DTA DER AM-VERS           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    22.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.09.98! 01 ! NEUERSTELLUNG             ! SCHEIDEL *
      * AFPA0012!07.11.98! 02 ! ANPASSUNG AN NEUES DTA DER! SCHEIDEL *
      *         !        !    ! AM-VERS                   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE STRUKTUR DES VON DER AM-VERS      *
      *    GELIEFERTEN BESTANDS-DTAS                                 *
      ****************************************************************
      *
          20 BST-DTA-SATZ.
           25 BST-VNR                  PIC  9(014).
           25 FILLER                   PIC  X(001).
           25 BST-VNR-ALT              PIC  9(014).
           25 FILLER                   PIC  X(001).
           25 BST-PIN                  PIC  9(007).
           25 FILLER                   PIC  X(004).
071198*    25 BST-NAME1                PIC  X(021).
071198     25 BST-NAME1                PIC  X(020).
           25 FILLER                   PIC  X(001).
071198*    25 BST-NAME2                PIC  X(021).
071198     25 BST-NAME2                PIC  X(025).
           25 FILLER                   PIC  X(001).
           25 BST-STORNODAT            PIC  9(006).
           25 FILLER                   PIC  X(001).
           25 BST-DVAG-DIREKTION       PIC  9(003).
           25 FILLER                   PIC  X(001).
           25 BST-DVAG-VBNR            PIC  9(006).
           25 FILLER                   PIC  X(001).
           25 BST-ZAHLWEISE            PIC  9(002).
           25 FILLER                   PIC  X(001).
           25 BST-NAECHSTE-FAELLIGKEIT PIC  X(010).
           25 FILLER                   PIC  X(001).
           25 BST-VERTRAGSBEGINN       PIC  X(010).
           25 FILLER                   PIC  X(001).
           25 BST-VERTRAGSABLAUF       PIC  X(010).
           25 FILLER                   PIC  X(001).
           25 BST-PG-VERTRAGSSTAND     PIC  9(002).
           25 FILLER                   PIC  X(001).
           25 BST-PG-ABGANGSGRUND      PIC  X(009).
           25 FILLER                   PIC  X(001).
           25 BST-WAEHRUNG             PIC  X(003).
           25 FILLER                   PIC  X(001).
           25 BST-MAX-USED-PROD        PIC  9(002).
           25 BST-PRODUKTE             OCCURS 050 TIMES.
            30 FILLER                  PIC  X(001).
            30 BST-TARIF               PIC  X(010).
            30 BST-JAHRESNETTOPRAEMIE  PIC  9(007)V9(02).
