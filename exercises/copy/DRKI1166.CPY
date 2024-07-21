      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1166.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1166.CPv  $
      *
      *   Rev 2.5   Jan 09 2020 13:50:06   t13ex39
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank
      *
      *   Rev 2.4   Dec 01 2016 06:10:18   t13ex39
      *AKI01459: DEUBA  Migration PK-Kredit Bankprodukt GHP DBProdukt
      *
      *   Rev 2.3   Oct 06 2016 12:03:26   t13ex39
      *AKI01459: DEUBA  Migration PK-Kredit Bankprodukt GHP DBProdukt
      *
      *   Rev 2.2   Jan 16 2003 13:35:08   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.1   31 Jan 2001 14:25:58   T13EX39
      *AKI00068 KBI
      *
      *   Rev 2.0   07 Jun 2000 17:08:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1166                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1166                    *
      *    --------------------------------------------------------   *
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!10.01.01! NEU!                           ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00303!15.10.02!    ! zus. Attribut             ! WHO      *
      *         !        !    ! PO-DRKI1166-VRBKOMMBETRKZ !          *
      *         !        !    ! aufgenommen               !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1166
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1166.
           05  PI-DRKI1166-KEY.
             10 PI-DRKI1166-VRBVERTRAGSID               PIC X(13).
             10 PI-DRKI1166-VRBPERSONENROLLEKZ          PIC X(01).
      *
       01  PO-DRKI1166.
           05  PO-DRKI1166-RC                           PIC  X(003).
               88 PO-DRKI1166-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1166-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1166-DATEN.
             10 PO-DRKI1166-VRBPERSONENID                PIC X(13).
             10 PO-DRKI1166-VRBKOMMBETRKZ                PIC X(01).
      *
      ******* ENDE COPY-MEMBER **************************************



