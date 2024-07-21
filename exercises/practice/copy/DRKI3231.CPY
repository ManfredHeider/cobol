      *****************************************************************
      *    COPY-MEMBER: DRKI3231                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3231                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01534!11.12.17!2.1 ! NEUERSTELLUNG             ! GWA
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01819!20.01.21! MIA: NEUE FELDER HINZU:         ! T13AC20
      *         !        !      VZUST-D                    !
      *         !        ! [PVCS 2.3]                      !
      * DBO8173 !17.03.21! MIA: FELDER ENTFALLEN:          ! T13AC20    
      *         !        !      RZINS                      !
      * --------!--------!---------------------------------!----------- 
      *         !        !                                 !            
      *         !        !                                 !            
      *                                                                 
      * ENDE-LOGBUCH                                                    
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3231
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3231.
           05 PI-DRKI3231-UFKT-CODE               PIC  9(002).
              88 PI-DRKI3231-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI3231-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI3231-UFKT-QUIT                  VALUE 3.
      *
           05  PI-DRKI3231-KEY.
                10 PI-DRKI3231-G20VMANDANT        PIC S9(9) BINARY.
      *
       01  PO-DRKI3231.
           05 PO-DRKI3231-RC                      PIC  X(003).
              88 PO-DRKI3231-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3231-RC-LEER                     VALUE 'LER'.
              88 PO-DRKI3231-RC-EOF                      VALUE 'EOF'.
      *
           05 PO-DRKI3231-DATEN.
              10 PO-DRKI3231-G20TVNR              PIC  X(0015).
              10 PO-DRKI3231-G20TLFDNR            PIC S9(4) BINARY.
              10 PO-DRKI3231-G20TVERARB-KZN       PIC  X(0001).
              10 PO-DRKI3231-G20TBEZUGLFDNR       PIC S9(5)V
                                                        PACKED-DECIMAL.
              10 PO-DRKI3231-G20THVT-KZN          PIC  X(0001).
              10 PO-DRKI3231-G20TTARIFKLASSE      PIC S9(5)V
                                                        PACKED-DECIMAL.
              10 PO-DRKI3231-G20TBEGINN           PIC  X(0010).
              10 PO-DRKI3231-G20TABLAUF           PIC  X(0010).
              10 PO-DRKI3231-G20TBEITRZUSTID      PIC S9(4) BINARY.
              10 PO-DRKI3231-G20TVERSSUMME        PIC S9(9)V99
                                                        PACKED-DECIMAL.
              10 PO-DRKI3231-G20TABLBZ            PIC  X(0010).
              10 PO-DRKI3231-G20TTARIFBEITRAG     PIC S9(9)V99
                                                        PACKED-DECIMAL.
              10 PO-DRKI3231-G20TVZUST-D          PIC S9(4) BINARY.
      ******* ENDE COPY-MEMBER ***************************************
