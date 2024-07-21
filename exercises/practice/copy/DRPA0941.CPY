      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0941.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0941.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:51:12   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 14:16:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0941                                      *
      *    --------------------------------------------------------   *
      *    MELD-TEIL                                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.04.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT EINEN TEIL DER BESTANDSMELDUNGSDATEN  *
      *    FÜR DIE ERSTELLUNG DER KONTROLLWERTE DES KONTROLLSYSTEMS  *
      *                                                              *
      ****************************************************************
      *
             30 MELD-TEIL.
                35 PDTA-ID                          PIC S9(015)
                                                        PACKED-DECIMAL.
                35 PGEB-GES                         PIC S9(004) BINARY.
                35 KNR-WAEHRUNG                     PIC S9(004) BINARY.
                35 VSTAT-KLASSE-MELD                PIC  X(001).
      *
                35 MAX-USED-PMPR-TEIL               PIC S9(004) BINARY.
                35 PMPR-TEIL                        OCCURS 15.
                   40 PROD-PROD-SCHL-PMPR           PIC S9(004) BINARY.
                   40 VSTAT-KLASSE-PMPR             PIC  X(001).
                   40 PMPR-PG-GSMT-PROV             PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
                35 MAX-USED-PMPT-TEIL               PIC S9(004) BINARY.
                35 PMPT-TEIL                        OCCURS 80.
                   40 PROD-PROD-SCHL-PMPT           PIC S9(004) BINARY.
                   40 VSTAT-KLASSE-PMPT             PIC  X(001).
                   40 PMPT-GSMT-PROV                PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
