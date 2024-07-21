      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0231.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0231.CPv  $
      *
      *   Rev 2.3   05 Dec 2007 10:46:12   t13ex27
      *AKL00664: Allfinanz: Mandantenprüfung bei ID-Funktionen
      *
      *   Rev 2.2   30 Jan 2001 08:50:14   T13EX13
      *FKI431- Löschen 6er VBs Beziehungen zu
      *den zu löschenden PC-Nummern
      *
      *   Rev 1.1   12 Aug 1998 16:48:22   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0231  FUNKTIONEN STANDARD-OZP            *
      *                           MITABREITER                        *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    13.07.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
001206* FKI00431!06.12.00!    ! Neue Funktion DRKI1154    ! Blaess   *
      * AKL00664!28.11.07!2.3 ! neue Funktion             !I. Frenzel
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0068.
           05 PI-ZTKI0068-FUNKTION             PIC X(008).
              88 PI-ZTKI0068-DRKI0232-SEL-ALL      VALUE 'DRKI0232'.
              88 PI-ZTKI0068-DRKI0233-UPD          VALUE 'DRKI0233'.
              88 PI-ZTKI0068-DRKI0234-INS          VALUE 'DRKI0234'.
001206        88 PI-ZTKI0068-DRKI1154-DEL          VALUE 'DRKI1154'.
281107        88 PI-ZTKI0068-DRKI2040-SEL-ALL      VALUE 'DRKI2040'.
