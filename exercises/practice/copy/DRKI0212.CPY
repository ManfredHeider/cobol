      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0212.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI0212.CPv  $
      *
      *   Rev 2.6   May 12 2015 09:19:22   t13ex73
      *AKI01360: §34f VB-SST Umsetzung
      *
      *   Rev 2.5   13 Feb 2001 09:02:22   T13EX49
      *AKI00144 - Rückstufungsdatum
      *
      *   Rev 2.4   Jun 28 1999 12:58:24   T13EX19
      *fki00070 - neue fkt DRKI0877
      *
      *   Rev 2.3   Jun 01 1999 12:04:26   T13EX19
      *                  neue fkt DRKI0824
      *
      *   Rev 2.2   May 21 1999 11:59:58   T13EX19
      *-alte version 2.0 wieder in Prod
      *
      *   Rev 2.0   Feb 27 1999 12:53:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:47:36   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:48:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0212  FUNKTIONEN STANDARD-OZP            *
      *                           KOMMUNIKATION                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    14.07.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!14.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !21.05.99!    ! neue fkt DRKI0824         ! Zgur-Vujic
      *         !24.06.99!    ! neue fkt DRKI0877         ! Zgur-Vujic
250900* AKI00144!25.09.00!    ! Neue Funktion DRKI1129    ! P. Pohl  *
      * -------------------------------------------------------------*
230415* AKI01360!23.04.15! 07 ! Neue Funktion DRKI3003    !DDY       *
      *         !        !    ! UPD auf ADMA34FDAT.       !          *
      *         !        !    ! DRKI0824 von Update auf   !          *
      *         !        !    ! Select-VBSST umbauen.     !          *
      *         !        !    ! DRKI0877 ausbauen.        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0060.
           05 PI-ZTKI0060-FUNKTION             PIC X(008).
              88 PI-ZTKI0060-DRKI0213-SEL-ALL      VALUE 'DRKI0213'.
              88 PI-ZTKI0060-DRKI0214-INS          VALUE 'DRKI0214'.
              88 PI-ZTKI0060-DRKI0215-UPD          VALUE 'DRKI0215'.
              88 PI-ZTKI0060-DRKI0216-UPD          VALUE 'DRKI0216'.
              88 PI-ZTKI0060-DRKI0217-UPD          VALUE 'DRKI0217'.
              88 PI-ZTKI0060-DRKI0218-UPD          VALUE 'DRKI0218'.
              88 PI-ZTKI0060-DRKI0219-UPD          VALUE 'DRKI0219'.
              88 PI-ZTKI0060-DRKI0220-UPD          VALUE 'DRKI0220'.
230415        88 PI-ZTKI0060-DRKI0824-SEL          VALUE 'DRKI0824'.
250900        88 PI-ZTKI0060-DRKI1129-UPD          VALUE 'DRKI1129'.
230415        88 PI-ZTKI0060-DRKI3003-UPD          VALUE 'DRKI3003'.

