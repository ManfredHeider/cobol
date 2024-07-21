      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0192.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0192.CPv  $
      *
      *   Rev 2.4   Jan 23 2007 16:23:26   t13ex14
      *AKI00556: Ablösung PERSON_OKI_REF (UPKI)
      *
      *   Rev 2.3   Oct 09 2006 16:46:34   t13ex14
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Personen
      *
      *   Rev 2.2   Oct 06 2006 11:48:02   t13ex14
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Personen
      *
      *   Rev 2.1   Mar 30 2000 11:05:36   T13EX39
      *AKI00098 OZPs
      *
      *   Rev 1.2   29 Oct 1998 17:46:50   T13EX19
      *FKI00010 - Korrektion
      *
      *   Rev 1.1   12 Aug 1998 16:46:38   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   29 Sep 1997 11:56:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0192  FUNKTIONEN STANDARD-OZP            *
      *                           PERSON                             *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    08.07.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!08.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !16.10.98! 02 ! DELETE                    !  gbe     *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00098!13.03.00!1.0 ! SELECT (DRKI1043)         ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00556!12.09.06!2.0 ! SELECT (DRKI1952)         ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0043.
           05 PI-ZTKI0043-FUNKTION             PIC X(008).
              88 PI-ZTKI0043-DRKI0194-INS          VALUE 'DRKI0194'.
              88 PI-ZTKI0043-DRKI0195-UPD          VALUE 'DRKI0195'.
              88 PI-ZTKI0043-DRKI0193-SEL-ALL      VALUE 'DRKI0193'.
              88 PI-ZTKI0043-DRKI0736-DEL          VALUE 'DRKI0736'.
              88 PI-ZTKI0043-DRKI1043              VALUE 'DRKI1043'.
              88 PI-ZTKI0043-DRKI1952              VALUE 'DRKI1952'.
      *
      ****************************************************************