      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0567.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0567.CPv  $
      *
      *   Rev 2.4   Feb 27 2007 12:46:52   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.3   Mar 28 2005 11:17:30   t13ex49
      *FKI00927: Adresskorrekturen
      *
      *   Rev 2.2   Feb 02 2004 07:35:40   t13ex49
      *AKI00114 - Änderung der Tabelle ADRESSAENDERUNG
      *
      *   Rev 2.1   Oct 27 2003 14:53:40   t13ex49
      *h:\mfe\cpy\DRKI0567.CPY -> L:\mf\CPY\DRKI0567.CPv
      *Enter description of changes:
      *Tabellenänderung für AdressAenderung
      *
      *   Rev 2.0   Feb 27 1999 12:57:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Feb 1999 11:47:40   T13EX49
      *AKI00014 - Funktionen ADR-Aenderung
      *
      *   Rev 1.0   07 Aug 1998 14:48:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0567  FUNKTIONSAUFRUFE FUER ADRESSAENDNG.*
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    17.08.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      * -------------------------------------------------------------*
      * AKI00001!05.11.98! 02 ! Zusätzliche Funktionen:   !v.d.Goltz *
      *         !        !    ! List nach Adresse-alt und !          *
      *         !        !    ! List nach Adresse-neu.    !          *
161003* AKI00114!16.10.03!    ! DRKI1593 - List nach      !P. Pohl   *
161003*         !        !    ! PersonenID, VertragsID,   !          *
161003*         !        !    ! AdresseAltID              !          *
161003*         !        !    ! DRKI1596 - List nach      !P. Pohl   *
161003*         !        !    ! ADAADRESSENEUID und       !          *
161003*         !        !    ! ADAPERSONENID             !          *
      * -------------------------------------------------------------*
260104* AKI00114!26.01.04!    ! neue Funktion DRKI1632    !P. Pohl   *
      * -------------------------------------------------------------*
250305* FKI00927!25.03.05!    ! neue Funktion:            !P. Pohl   *
250305*         !        !    ! DRKI1774 - ADRESSENEUID   !          *
250305*         !        !    ! DRKI1775 - ADRESSEALTID   !          *
      * -------------------------------------------------------------*
      * AKI00557!29.01.07!    ! neue Funktion             !I. Frenzel*
      *         !        !    ! DRKI1862 - ADAPERSONENID  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXKI0079.
           05 PI-ZXKI0079-FUNKTION            PIC X(008).
              88 PI-ZXKI0079-DRKI0568-LIST        VALUE 'DRKI0568'.
051198        88 PI-ZXKI0079-DRKI0752-LIST        VALUE 'DRKI0752'.
051198        88 PI-ZXKI0079-DRKI0753-LIST        VALUE 'DRKI0753'.
161003        88 PI-ZXKI0079-DRKI1593             VALUE 'DRKI1593'.
161003        88 PI-ZXKI0079-DRKI1596             VALUE 'DRKI1596'.
260104        88 PI-ZXKI0079-DRKI1632             VALUE 'DRKI1632'.
250305        88 PI-ZXKI0079-DRKI1774             VALUE 'DRKI1774'.
250305        88 PI-ZXKI0079-DRKI1775             VALUE 'DRKI1775'.
290107        88 PI-ZXKI0079-DRKI1862             VALUE 'DRKI1862'.

