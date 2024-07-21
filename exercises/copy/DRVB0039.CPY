      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0039.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0039.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1997 12:16:04   T13AE26
      *AVB00398 - 40Proz.Klausel Teil2
      *
      *   Rev 1.0   Nov 04 1995 09:10:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0039                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN DIALOGEN (CADD-I-KOMM-VAR, CADD-O-KOMM-VAR)       *
      *    FÜR XA05                                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    13.07.1993                                       *
      *****************************************************************
      *    VERSION   1    23.05.95 17:20:23
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !16.09.93! 01 ! NEU                       ! GIESE    *
      * FEVB0240!28.04.95!    ! KOMMENTAR                 ! GERLACH  *
      * AFVB0207!10.05.95! 02 ! KOMMENTAR                 ! GERLACH  *
      * AFVB0303!22.07.97!    ! UMBAU: INPUT- UND OUTPUT- ! SCHÖLLH. *
      *         !        !    !  SCHNITTSTELLE ZU XA05    !          *
      *         !        !    !  (DRVB0046 ENTFÄLLT)      !          *
      * AVB00398!25.11.97! 04 ! neue Auftragsnummer       ! T. MOll  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    PO-DRVB0039-TSQDD-NAME
      *    PO-DRVB0039-TSQ-SATZ-ANZ
      *     IM XA05 WERDEN ED-KONTEN ERZEUGT, DIESE WERDEN DURCH
      *     DAS UPRO UPVB0041 ALS TSQ-ITEMS WEGGESCHRIEBEN
      *     IM VB22 ERFOLGT ERST DER UPDATE AUF DIE DATENBANK, WENN DER
      *     ANWENDER SEIN OK GIBT. DER DATENBANK UPDATE ERFOLGT
      *     EBENFALLS DURCH DAS UPVB0041. DAFÜR WIRD BENÖTIGT
      *     - DER NAME DER TSQ
      *     - DIE ANZAHL DER 'GEPARKTEN' ITEMS
      *
      *    PO-DRVB0039-S-ED              J/N
      *     SCHALTER: WENN EIN DIREKT VON DER 800000 BETREUTER VB
      *     ZUM D BEFÖRDERT WIRD, DANN WIRD KEIN ED-KONTO ANGELEGT
      *
      **************************************************************

220796 01  PI-DRVB0039.
220796     05  PI-DRVB0039-CADD-I-KOMM-VAR            PIC  X(128).
220796     05  FILLER REDEFINES   PI-DRVB0039-CADD-I-KOMM-VAR.
220796
220796         10  PI-DRVB0039-FUNKTION               PIC  X(008).
220796             88 PI-DRVB0039-ED-ANLEGEN-XA05     VALUE 'DRVB0039'.
220796
220796         10  PI-DRVB0039-CICS-TIMESTAMP         PIC  X(026).
220796*        **  die vb-nr wird über CADD-OB-VB-NR übergeben

220796*      15 Z-DRVB0039-ED-KONTO          PIC  X(01).
220796*      15 Z-DRVB0039-DIR-NR-X          PIC  X(04).
220796*      15 Z-DRVB0039-CICS-TIMESTAMP    PIC  X(26).
      *
220796 01  PO-DRVB0039.
220796     05  PO-DRVB0039-CADD-O-KOMM-VAR            PIC  X(128).
220796     05  FILLER REDEFINES   PO-DRVB0039-CADD-O-KOMM-VAR.
220796
220796         10  PO-DRVB0039-TSQDD-NAME             PIC  X(08).
220796         10  PO-DRVB0039-TSQ-SATZ-ANZ           PIC  9(04).
220796         10  PO-DRVB0039-ED                     PIC  X(01).
220796             88  PO-DRVB0039-ED-ANGELEGT-JA     VALUE 'J'.
220796             88  PO-DRVB0039-ED-ANGELEGT-NEIN   VALUE 'N'.
      *
      ******** ENDE COPY-MEMBER (DRVB0039) *******************
